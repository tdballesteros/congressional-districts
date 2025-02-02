
# This file computes Ohio's 16 congressional districts using the 2010 US Census data and 2020
# election results, which used districts drawn from the 2010 Census data.


### load libraries ----------------------------------------------------------------------
library(readxl)
library(sf)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

## Adjacency List
# Source: Derived from US Census Bureau and TIGER/Line
adjacency_list <- read.csv(
  "Data/Calculated_Adjacency_List_Tracts_2010.csv",
  colClasses = "character"
  )

# v2 removes six unpopulated tracts in Lake Erie
adjacency_list_v2 <- read.csv(
  "Data/Calculated_Adjacency_List_v2_Tracts_2010.csv",
  colClasses = "character"
  )

## Population Data
# Source: US Census Bureau
population_data <- read.csv(
  "Data/population_data_2010_by_tract.csv",
  skip = 1,
  colClasses = "character"
  )

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10)


### format data ----------------------------------------------------------------------
pop_tracts_total <- population_data %>%
  dplyr::filter(
    # Ohio FIPS code is 39
    stringr::str_sub(Geography, 10, 11) == "39",
    # filter out total row for Ohio
    Geography != "0400000US39"
    ) %>%
  dplyr::select(Geography, Total) %>%
  dplyr::rename(Population = Total) %>%
  dplyr::mutate(
    Geography = stringr::str_sub(Geography, 10),
    # several tracts have non-numeric entries in the Population  field; manually
    # correct these and convert all data to numeric values
    Population = dplyr::case_when(
      Geography %in% c(
        "39035118800", "39035141300", "39035187105", "39035187106", "39035195900",
        "39055310200", "39055310600", "39055310800", "39055310900", "39055311000",
        "39055311300", "39055311400", "39055311600", "39055311700", "39055311800",
        "39055312100", "39055312202", "39055312300", "39055312400", "39085206400",
        "39155930500") ~ as.numeric(stringr::str_sub(Population, 1, 4)),
      .default = as.numeric(Population)
    )
  )

pop_tracts_total_v2 <- pop_tracts_total %>%
  dplyr::filter(Geography %!in% c(
    "39095990000", "39043990100", "39093990200", "39035990000", "39085990000", "39007990000"
    ))


# create modified population data by merging tract enclaves with their surrounding tract,
# as these tracts will always need to be within the same district

# SOURCE_TRACTID is the enclave, NEIGHBOR_TRACTID is the surrounding tract
tract_enclave_df <- adjacency_list %>%
  dplyr::group_by(SOURCE_TRACTID) %>%
  dplyr::filter(n() == 1) %>%
  dplyr::ungroup()

# additional enclaves
tract_enclaves_to_add <- data.frame(
  ENCLAVE_TRACTID = c(
    # Oxford enclave
    "39017010102", "39017010103", "39017010101", "39017010104", "39017010201",
    # Marins Ferry enclave
    "39013012000", "39013011900", "39013012100",
    # Bellaire enclave
    "39013011500", "39013011700", "39013011600",
    # Toronto enclave
    "39081011200", "39081011300",
    # Port Clinton enclave
    "39123050600", "39123050500"
    ),
  SURROUNDING_TRACTID = c(
    # Oxford enclave
    rep("39017010202", 5),
    # Marins Ferry enclave
    rep("39013010100", 3),
    # Bellaire enclave
    rep("39013011400", 3),
    # Toronto enclave
    rep("39081011401", 2),
    rep("39123051200", 2)
    )
  )
  
tract_enclave_df <- tract_enclave_df %>%
  dplyr::rename(
    ENCLAVE_TRACTID = SOURCE_TRACTID,
    SURROUNDING_TRACTID = NEIGHBOR_TRACTID
  ) %>%
  rbind(tract_enclaves_to_add)

tract_enclave_df_v2 <- tract_enclave_df %>%
  # Conneaut enclave, only an enclave in v2 of the adjacency data
  tibble::add_row(
    ENCLAVE_TRACTID = "39007000102",
    SURROUNDING_TRACTID = "39007000101"
  ) %>%
  tibble::add_row(
    ENCLAVE_TRACTID = "39007000103",
    SURROUNDING_TRACTID = "39007000101"
  )


### functions ----------------------------------------------------------------------

tractdist <- function(tracts, adjdf = adjacency_list, popdf = pop_tracts_total){
  
  # test for contiguity
  if(isContig(popdf$Geography, adjdf) == 0){
    return("Data not contiguous.")
  }
  
  # pull list of all tracts to find distance for
  tract_list <- unique(popdf$Geography)
  
  # list of calculated distances
  list_done <- c(tracts)
  
  # list of uncalculated distances
  list_todo <- c(tract_list)
  
  # remove tracts from the list
  list_todo <- list_todo[!list_todo %in% tracts]
  
  # set up distance counter
  d <- 1
  
  # set up output df
  output <- popdf %>%
    dplyr::select(-Population) %>%
    dplyr::mutate(dist = NA)
  
  # set tract1 dist equal to 0
  output$dist[output$Geography %in% tracts] <- 0
  
  x <- 1
  
  # while there are still distances to calculate...
  while(length(list_todo) > 0){

    # pull all tracts adjacent to a given tract
      next_circle <- adjdf %>%
        dplyr::filter(
          SOURCE_TRACTID %in% list_done,
          NEIGHBOR_TRACTID %!in% list_done
        ) %>%
        dplyr::pull(NEIGHBOR_TRACTID) %>%
        unique()
  
    output <- output %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dist = dplyr::case_when(
        is.na(dist) & Geography %in% next_circle ~ d,
        .default = dist
      ))
    
    list_done <- c(list_done, next_circle)
    list_todo <- list_todo[!list_todo %in% next_circle]
    d <- d + 1

  }
  
  return(output)
  
}


# test if all tracts in a given list are contiguous based
# on a given adjacency matrix
isContig <- function(list, adjdf = adjacency_list){
  
  list <- as.character(list)
  
  # select a random tract from the adjacency matrix as a starting point
  start <- sample(list,1)
  
  # create a list to add connected tracts to
  list_identified <- c(start)

  # set non-equal dummy values, later replaced by the number of
  # connected tracts (l2) and number of tracts not in the list
  # adjacent to those tracts (l3)
  l2 <- 1
  l3 <- 0
  
  # set dummy value, later replaced by the number of adjacent tracts
  # not already identified
  growth_length <- 1
  
  # while there are new tracts being added...
  while(growth_length != 0){
    
    growth_list <- adjdf %>%
      dplyr::filter(
        # adjacency for all tracts identified...
        SOURCE_TRACTID %in% list_identified,
        # ...but the neighbor is not identified
        !NEIGHBOR_TRACTID %in% list_identified) %>%
      dplyr::pull(NEIGHBOR_TRACTID) %>%
      unique()
    
    # add adjacent tracts to the list of identified tracts
    list_identified <- c(list_identified, growth_list)
    
    # calculate the number of newly added tracts
    growth_length <- length(growth_list)
    
  }
    
    if(length(list_identified) < length(list)){
      return(0)
    }
    else{
      return(1)
    }
  
}


identifyGroups <- function(tract_list, adjdf = adjacency_list){
  
  adjdf <- adjdf %>%
    dplyr::filter(
      SOURCE_TRACTID %in% tract_list,
      NEIGHBOR_TRACTID %in% tract_list)
  
  # list of all tracts to start with, with tracts removed from the list
  # as they are identified in a group
  list_unidentified <- tract_list
  list_identified <- list()
  
  # grouping index
  a <- 1
  
  tract_groupings <- list()
  
  while(length(list_unidentified) > 0){
    
    # set dummy value, later replaced by the number of adjacent tracts
    # not already identified
    growth_length <- 1
    start <- 1
    
    # pull a random tract in the list
    rand_tract <- sample(list_unidentified, 1)
    list <- c(rand_tract)
    
    # while there are new tracts being added...
    while(growth_length != 0){
      
      growth_list <- adjdf %>%
        dplyr::filter(
          # adjacency for all tracts identified...
          SOURCE_TRACTID %in% list,
          # ...but the neighbor is not identified
          NEIGHBOR_TRACTID %!in% list) %>%
        dplyr::pull(NEIGHBOR_TRACTID) %>%
        unique()
      
      # add adjacent tracts to the list of identified tracts
      list <- c(list, growth_list)
      
      # calculate the number of newly added tracts
      growth_length <- length(growth_list)
      
    }
    
    tract_groupings[[a]] <- list
    a <- a + 1
    
    list_identified <- c(list_identified, list) %>% unlist()
    list_unidentified <- list_unidentified[list_unidentified %!in% list_identified]
    
  }

  return(tract_groupings)
  
}


identifyEnclaves <- function(tract_list, adjdf = adjacency_list){
  
  tract_groupings <- identifyGroups(tract_list, adjdf)
  
  main_grouping_index <- which.max(sapply(tract_groupings, length))
  main_grouping_tracts <- tract_groupings[[main_grouping_index]]
  
  enclaves <- tract_list[tract_list %!in% main_grouping_tracts]
  
  return(enclaves)
  
}


splitIntoTwo <- function(df = pop_tracts_total, adjdf = adjacency_list, version = 1){
  
  # STEPS
  ## Step 0: Pre-Processing
  ## Step 1: Initial Random Tracts
  ## Step 2: Starting Tract Groupings
  ## Step 3: Expand Halves
  ## Step 4: Assign Unassigned Tracts
  ## Step 5: Reassignments
  ## Step 6: Add Enclave Data
  ## Step 7: Finalize Data
  

  ##### STEP 0: PRE-PROCESSING
  ##### Test for contiguity, remove enclaves and recalculate populations, and reconfirm contiguity.
  
  # test if the input df is contiguous
  if(isContig(df$Geography, adjdf) == 0){
    
    stop(paste("Error: Input Not Contiguous"))
    
  }
  
  # pull master enclave list based on version (including or excluding several water tracts)
  if(version == 1){
    master_enclave_data <- tract_enclave_df
  } else if(version == 2){
    master_enclave_data <- tract_enclave_df_v2
  }
  
  master_enclave_list <- master_enclave_data$ENCLAVE_TRACTID
  master_surrounding_list <- master_enclave_data$SURROUNDING_TRACTID
  
  # pull enclaves within the inputted data
  enclaves_list <- df$Geography[df$Geography %in% master_enclave_list]
  
  # pull surrounding tracts within the inputted data
  surrounding_list <- df$Geography[df$Geography %in% master_surrounding_list]
  
  # create a modified population df merging the enclaves and their populations into
  # their respective surrounding districts in order to ensure they are assigned together
  df_mod <- df %>%
    dplyr::left_join(master_enclave_data,
                     dplyr::join_by("Geography" == "ENCLAVE_TRACTID")) %>%
    dplyr::mutate(Geography = dplyr::coalesce(SURROUNDING_TRACTID, Geography)) %>%
    dplyr::group_by(Geography) %>%
    dplyr::summarise(Population = sum(Population, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # modify adjacency list to remove all enclaves
  adjdf_mod <- adjdf %>%
    dplyr::filter(
      SOURCE_TRACTID %!in% enclaves_list,
      NEIGHBOR_TRACTID %!in% enclaves_list
    )
  
  # test if the modified data is contiguous
  if(isContig(df_mod$Geography, adjdf_mod) == 0){
    
    stop(paste("Error: Input Not Contiguous"))
    
  }
  
  ##### STEP 1: INITIAL RANDOM TRACTS
  ##### Select two random tracts, calculate distances from each of those two tracts by the minimum number of
  ##### tracts between them, and calculate the difference of those distances to measure how relatively closer
  ##### each tract is from the two initial random tracts.
  
  # list all unassigned districts in the population
  tract_list <- unique(df_mod$Geography)
  
  # select random tract from population
  tract1 <- sample(tract_list, 1)
  
  # exclude tract1 from the sample
  tract_list <- tract_list[!tract_list %in% tract1]
  
  # select second random tract from the new population
  tract2 <- sample(tract_list,1)
  
  # calculate distances to tract1
  t1_df <- tractdist(as.character(tract1), adjdf_mod, df_mod) %>%
    dplyr::rename(dist1 = dist)
  # calculate distances to tract2
  t2_df <- tractdist(tract2, adjdf_mod, df_mod) %>%
    dplyr::rename(dist2 = dist)
  
  # join the two distance columns together
  tdf <- dplyr::full_join(t1_df, t2_df,
                          by = c("Geography")) %>%
    # calculate differences of distances
    ## positive - tract2 is closer
    ## negative - tract1 is closer
    dplyr::mutate(
      distfrom = dist2 - dist1,
      Geography = as.character(Geography)
    )
  
  # merge in tract population estimates
  tdf <- dplyr::left_join(tdf, df_mod, by=c("Geography")) %>%
    dplyr::mutate(Population = as.numeric(Population))
  
  ##### STEP 2: STARTING TRACT GROUPINGS
  ##### Identify tracts with the most extreme difference of distances (i.e., the most relatively far from each
  ##### initial random tract), select the largest group of contiguous tracts within each group, and set those
  ##### two groups as the starting points for each half.
  
  # pull the most extreme distfrom values and pull the tracts in the largest contiguous group
  # or pick one at random if all are disconnected
  distfrom_low <- min(tdf$distfrom, na.rm = TRUE)
  distfrom_high <- max(tdf$distfrom, na.rm = TRUE)
  
  # starting point Half 1
  start_half1 <- tdf %>%
    dplyr::filter(distfrom == distfrom_low)
  
  start_half1 <- identifyGroups(start_half1$Geography, adjdf_mod)
  
  # identify the group with the largest number of tracts in it, or the first group with the max
  # length in the case of ties
  largest_grouping_index_half1 <- which.max(sapply(start_half1, length))
  starting_half1_list <- start_half1[[largest_grouping_index_half1]]
  
  # starting point Half 2
  start_half2 <- tdf %>%
    dplyr::filter(distfrom == distfrom_high)
  
  start_half2 <- identifyGroups(start_half2$Geography, adjdf_mod)
  
  # identify the group with the largest number of tracts in it, or the first group with the max
  # length in the case of ties
  largest_grouping_index_half2 <- which.max(sapply(start_half2, length))
  starting_half2_list <- start_half2[[largest_grouping_index_half2]]
  
  # set list of tracts assigned to each half to the starting lists
  half1_tract_list <- c(starting_half1_list)
  half2_tract_list <- c(starting_half2_list)
  
  # set list of assigned and unassigned tracts
  assigned_tract_list <- c(half1_tract_list, half2_tract_list)
  unassigned_tract_list <- tdf %>%
    dplyr::filter(Geography %!in% c(half1_tract_list, half2_tract_list)) %>%
    dplyr::pull(Geography)
  
  # set cumulative population sums of assigned tracts to the starting value populations
  half1_population <- tdf %>%
    dplyr::filter(Geography %in% half1_tract_list) %>%
    dplyr::pull(Population) %>%
    sum()
  
  half2_population <- tdf %>%
    dplyr::filter(Geography %in% half2_tract_list) %>%
    dplyr::pull(Population) %>%
    sum()
  
  ##### STEP 3: EXPAND HALVES
  ##### Continue to add adjacent districts to each half until one of the halves reaches the
  ##### population target goal
  
  # calculate the sum goal
  sum_goal <- (sum(tdf$Population, na.rm = TRUE) / 2) - median(tdf$Population, na.rm = TRUE)
  
  # set distfrom counters starting at their lowest/highest values
  distfrom_lower_counter <- distfrom_low + 1
  distfrom_higher_counter <- distfrom_high - 1
  
  while((half1_population < sum_goal) & (half2_population < sum_goal)){
    
    # subset adjacency lists
    adjdf_half1 <- adjdf_mod %>%
      dplyr::filter(
        SOURCE_TRACTID %in% half1_tract_list,
        NEIGHBOR_TRACTID %!in% half1_tract_list
      )
    
    adjdf_half2 <- adjdf_mod %>%
      dplyr::filter(
        SOURCE_TRACTID %in% half2_tract_list,
        NEIGHBOR_TRACTID %!in% half2_tract_list
      )
    
    # pull eligible tracts to add
    tmp_half1 <- tdf %>%
      dplyr::rowwise() %>%
      # create binary adjacency flags
      dplyr::mutate(
        half1_adj = dplyr::case_when(
          Geography %in% adjdf_half1$NEIGHBOR_TRACTID ~ 1,
          .default = 0
        )
      ) %>%
      dplyr::filter(
        Geography %!in% assigned_tract_list,
        half1_adj == 1,
        distfrom <= distfrom_lower_counter
      ) %>%
      dplyr::arrange(desc(dist1),dist2)
    
    tmp_half2 <- tdf %>%
      dplyr::rowwise() %>%
      # create binary adjacency flags
      dplyr::mutate(
        half2_adj = dplyr::case_when(
          Geography %in% adjdf_half2$NEIGHBOR_TRACTID ~ 1,
          .default = 0
        )
      ) %>%
      dplyr::filter(
        Geography %!in% assigned_tract_list,
        half2_adj == 1,
        distfrom >= distfrom_higher_counter
      )
    
    # calculate population
    half1_population_tmp <- half1_population + sum(tmp_half1$Population, na.rm = TRUE)
    half2_population_tmp <- half2_population + sum(tmp_half2$Population, na.rm = TRUE)
    
    # if the population puts it over the target, only pull part of the list
    if(half1_population_tmp >= sum_goal){
      
      pop_difference <- sum_goal - half1_population
      
      tmp_half1 <- tmp_half1 %>%
        dplyr::group_by() %>%
        dplyr::mutate(cumsum = cumsum(Population)) %>%
        dplyr::ungroup()
      
      index_half1 <- dplyr::case_when(
        tmp_half1$cumsum[1] >= pop_difference ~ 1,
        tmp_half1$cumsum[1] < pop_difference ~ max(which(tmp_half1$cumsum < pop_difference)) + 1,
        .default = 1
      )
      
      half1_add_list <- tmp_half1 %>%
        dplyr::filter(row_number() <= index_half1) %>%
        dplyr::pull(Geography)
      
    } else{
      
      half1_add_list <- tmp_half1 %>%
        dplyr::pull(Geography)
      
    }
    
    if(half2_population_tmp >= sum_goal){
      
      pop_difference <- sum_goal - half2_population
      
      tmp_half2 <- tmp_half2 %>%
        dplyr::group_by() %>%
        dplyr::mutate(cumsum = cumsum(Population)) %>%
        dplyr::ungroup()
      
      index_half2 <- dplyr::case_when(
        tmp_half2$cumsum[1] >= pop_difference ~ 1,
        tmp_half2$cumsum[1] < pop_difference ~ max(which(tmp_half2$cumsum < pop_difference)) + 1,
        .default = 1
      )      
      
      half2_add_list <- tmp_half2 %>%
        dplyr::filter(row_number() <= index_half2) %>%
        dplyr::pull(Geography)
      
    } else{
      
      half2_add_list <- tmp_half2 %>%
        dplyr::pull(Geography)
      
    }
    
    # if the tract is in both lists
    tracts_to_add_in_both <- base::intersect(half1_add_list, half2_add_list)
    
    if(length(tracts_to_add_in_both) > 0){
      tracts_in_both_df <- data.frame(Geography = tracts_to_add_in_both) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          half1_adj = adjdf_mod %>%
            dplyr::filter(
              SOURCE_TRACTID == Geography,
              NEIGHBOR_TRACTID %in% half1_tract_list
            ) %>%
            nrow(),
          half2_adj = adjdf_mod %>%
            dplyr::filter(
              SOURCE_TRACTID == Geography,
              NEIGHBOR_TRACTID %in% half2_tract_list
            ) %>%
            nrow(),
          total_adj = half1_adj + half2_adj,
          half1_adj_perc = half1_adj / total_adj,
          half2_adj_perc = half2_adj / total_adj,
          half_assigned = dplyr::case_when(
            half1_adj_perc > half2_adj_perc ~ 1,
            half2_adj_perc > half1_adj_perc ~ 2,
            .default = NA
          )
        )
      
      # remove all tracts in both lists from each list
      half1_add_list <- half1_add_list[half1_add_list %!in% tracts_to_add_in_both]
      half2_add_list <- half2_add_list[half2_add_list %!in% tracts_to_add_in_both]
      
      # pull new assignments for tracts in both lists
      half1_add_list_plus <- tracts_in_both_df %>%
        dplyr::filter(half_assigned == 1) %>%
        dplyr::pull(Geography)
      
      half2_add_list_plus <- tracts_in_both_df %>%
        dplyr::filter(half_assigned == 2) %>%
        dplyr::pull(Geography)
      
      # add new assignments to list of additions
      half1_add_list <- c(half1_add_list, half1_add_list_plus)
      half2_add_list <- c(half2_add_list, half2_add_list_plus)
      
    }
    
    # calculate population
    add_half1_population <- tdf %>%
      dplyr::filter(Geography %in% half1_add_list) %>%
      dplyr::pull(Population) %>%
      sum()
    
    add_half2_population <- tdf %>%
      dplyr::filter(Geography %in% half2_add_list) %>%
      dplyr::pull(Population) %>%
      sum()
    
    half1_population <- half1_population + add_half1_population
    half2_population <- half2_population + add_half2_population
    
    # combine tracts to add with list
    half1_tract_list <- c(half1_tract_list, half1_add_list)
    half2_tract_list <- c(half2_tract_list, half2_add_list)
    assigned_tract_list <- c(assigned_tract_list, half1_add_list, half2_add_list)
    unassigned_tract_list <- unassigned_tract_list[unassigned_tract_list %!in% assigned_tract_list]
    
    distfrom_lower_counter <- distfrom_lower_counter + 1
    distfrom_higher_counter <- distfrom_higher_counter - 1
    
  }
  
  ##### STEP 4: ASSIGN UNASSIGNED TRACTS
  ##### Assign unassigned tracts from the previous step to whichever half did not reach the population goal.
  
  # assign unassigned tracts to the half with the population below the threshold
  if(half1_population < sum_goal){
    
    half1_tract_list <- c(half1_tract_list, unassigned_tract_list)
    
  } else if(half2_population < sum_goal){
    
    half2_tract_list <- c(half2_tract_list, unassigned_tract_list)
    
  }
  
  tdf <- tdf %>%
    dplyr::mutate(half = dplyr::case_when(
      Geography %in% half1_tract_list ~ 1,
      Geography %in% half2_tract_list ~ 2,
      .default = NA
    ))
  
  ##### STEP 5: REASSIGNMENTS
  ##### Reassign enclaves and adjust for population goals on a loop.
  
  # repull list of tracts in each half
  half1_tract_list <- tdf %>%
    dplyr::filter(half == 1) %>%
    dplyr::pull(Geography)
  
  half2_tract_list <- tdf %>%
    dplyr::filter(half == 2) %>%
    dplyr::pull(Geography)
  
  # repull population of each half
  half1_population <- tdf %>%
    dplyr::filter(half == 1) %>%
    dplyr::pull(Population) %>%
    sum()
  
  half2_population <- tdf %>%
    dplyr::filter(half == 2) %>%
    dplyr::pull(Population) %>%
    sum()
  
  # calculate restricted adjacency lists
  adj_half_a <- adjdf_mod %>%
    dplyr::filter(
      SOURCE_TRACTID %in% half1_tract_list,
      NEIGHBOR_TRACTID %in% half1_tract_list
    )
  
  adj_half_b <- adjdf_mod %>%
    dplyr::filter(
      SOURCE_TRACTID %in% half2_tract_list,
      NEIGHBOR_TRACTID %in% half2_tract_list
    )
  
  # pull list of enclaves
  enclave_list_half_a <- identifyEnclaves(half1_tract_list, adj_half_a)
  enclave_list_half_b <- identifyEnclaves(half2_tract_list, adj_half_b)

  # while enclaves remain and population targets are not met
  while(length(enclave_list_half_a) > 0 | length(enclave_list_half_b) > 0 |
        (half1_population < sum_goal) | (half2_population < sum_goal)){
    
    # recode enclaves to the other half
    tdf <- tdf %>%
      dplyr::mutate(half = dplyr::case_when(
        Geography %in% enclave_list_half_a ~ 2,
        Geography %in% enclave_list_half_b ~ 1,
        .default = half
      ))
    
    # repull population counts
    half1_population <- tdf %>%
      dplyr::filter(half == 1) %>%
      dplyr::pull(Population) %>%
      sum()
    
    half2_population <- tdf %>%
      dplyr::filter(half == 2) %>%
      dplyr::pull(Population) %>%
      sum()
    
    # repull list of tracts in each half
    half1_tract_list <- tdf %>%
      dplyr::filter(half == 1) %>%
      dplyr::pull(Geography)
    
    half2_tract_list <- tdf %>%
      dplyr::filter(half == 2) %>%
      dplyr::pull(Geography)
    
    # reassign districts assigned to half 2 to half 1 so long as half 1's population is below the target
    while(half1_population < sum_goal){
      
      # calculate the population needed to reach the population goal for half 1
      pop_differential_half1 <- sum_goal - half1_population
      
      # calculate proportion of adjacent tracts in each half
      adding_to_half1 <- tdf %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          half1_adj = adjdf_mod %>%
            dplyr::filter(
              SOURCE_TRACTID == Geography,
              NEIGHBOR_TRACTID %in% half1_tract_list
            ) %>%
            nrow(),
          half2_adj = adjdf_mod %>%
            dplyr::filter(
              SOURCE_TRACTID == Geography,
              NEIGHBOR_TRACTID %in% half2_tract_list
            ) %>%
            nrow(),
          total_adj = half1_adj + half2_adj,
          half1_adj_perc = half1_adj / total_adj,
          half2_adj_perc = half2_adj / total_adj
        ) %>%
        dplyr::filter(
          half == 2,
          half1_adj > 0
        ) %>%
        dplyr::arrange(desc(half1_adj_perc), distfrom, desc(dist1), dist2) %>%
        dplyr::group_by() %>%
        dplyr::mutate(cumsum = cumsum(Population)) %>%
        dplyr::ungroup()
      
      # calculate how many of the (ordered) tracts are to be added to half 1, defaulting to 1 if the first tract's
      # population is greater than the population needed to reach the population target
      cutoff <- dplyr::case_when(
        adding_to_half1$cumsum[1] >= pop_differential_half1 ~ 1,
        adding_to_half1$cumsum[1] < pop_differential_half1 ~ max(which(adding_to_half1$cumsum < pop_differential_half1)) + 1,
        .default = 1
      )
      
      # pull only the tracts to add to half 1
      adding_to_half1 <- adding_to_half1 %>%
        dplyr::filter(row_number() <= cutoff)
      
      # recode the tracts into half 1
      tdf <- tdf %>%
        dplyr::mutate(half = dplyr::case_when(
          Geography %in% adding_to_half1$Geography ~ 1,
          .default = half
        ))
      
      # pull updated population
      half1_population <- tdf %>%
        dplyr::filter(half == 1) %>%
        dplyr::pull(Population) %>%
        sum()
      
      # repull tract lists
      half1_tract_list <- tdf %>%
        dplyr::filter(half == 1) %>%
        dplyr::pull(Geography)
      
      half2_tract_list <- tdf %>%
        dplyr::filter(half == 2) %>%
        dplyr::pull(Geography)
    }
    
    # pull updated population
    half2_population <- tdf %>%
      dplyr::filter(half == 2) %>%
      dplyr::pull(Population) %>%
      sum()
    
    # reassign districts assigned to half 1 to half 2 so long as half 2's population is below the target
    while(half2_population < sum_goal){
      
      # calculate the population needed to reach the population goal for half 2
      pop_differential_half2 <- sum_goal - half2_population
      
      # calculate proportion of adjacent tracts in each half
      adding_to_half2 <- tdf %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          half1_adj = adjdf_mod %>%
            dplyr::filter(
              SOURCE_TRACTID == Geography,
              NEIGHBOR_TRACTID %in% half1_tract_list
            ) %>%
            nrow(),
          half2_adj = adjdf_mod %>%
            dplyr::filter(
              SOURCE_TRACTID == Geography,
              NEIGHBOR_TRACTID %in% half2_tract_list
            ) %>%
            nrow(),
          total_adj = half1_adj + half2_adj,
          half1_adj_perc = half1_adj / total_adj,
          half2_adj_perc = half2_adj / total_adj
        ) %>%
        dplyr::filter(
          half == 1,
          half2_adj > 0
        ) %>%
        dplyr::arrange(desc(half2_adj_perc), distfrom, desc(dist2), dist1) %>%
        dplyr::group_by() %>%
        dplyr::mutate(cumsum = cumsum(Population)) %>%
        dplyr::ungroup()
      
      # calculate how many of the (ordered) tracts are to be added to half 2, defaulting to 1 if the first tract's
      # population is greater than the population needed to reach the population target
      cutoff <- dplyr::case_when(
        adding_to_half2$cumsum[1] >= pop_differential_half2 ~ 1,
        adding_to_half2$cumsum[1] < pop_differential_half2 ~ max(which(adding_to_half2$cumsum < pop_differential_half2)) + 1,
        .default = 1
      )
      
      # pull only the tracts to add to half 2
      adding_to_half2 <- adding_to_half2 %>%
        dplyr::filter(row_number() <= cutoff)
      
      # recode the tracts into half 2
      tdf <- tdf %>%
        dplyr::mutate(half = dplyr::case_when(
          Geography %in% adding_to_half2$Geography ~ 2,
          .default = half
        ))
      
      # pull updated population
      half2_population <- tdf %>%
        dplyr::filter(half == 2) %>%
        dplyr::pull(Population) %>%
        sum()
      
      # repull tract lists
      half1_tract_list <- tdf %>%
        dplyr::filter(half == 1) %>%
        dplyr::pull(Geography)
      
      half2_tract_list <- tdf %>%
        dplyr::filter(half == 2) %>%
        dplyr::pull(Geography)
      
    }
    
    # pull population and enclave information
    
    # repull list of tracts in each half
    half1_tract_list <- tdf %>%
      dplyr::filter(half == 1) %>%
      dplyr::pull(Geography)
    
    half2_tract_list <- tdf %>%
      dplyr::filter(half == 2) %>%
      dplyr::pull(Geography)
    
    # repull population of each half
    half1_population <- tdf %>%
      dplyr::filter(half == 1) %>%
      dplyr::pull(Population) %>%
      sum()
    
    half2_population <- tdf %>%
      dplyr::filter(half == 2) %>%
      dplyr::pull(Population) %>%
      sum()
    
    # calculate restricted adjacency lists
    adj_half_a <- adjdf_mod %>%
      dplyr::filter(
        SOURCE_TRACTID %in% half1_tract_list,
        NEIGHBOR_TRACTID %in% half1_tract_list
      )
    
    adj_half_b <- adjdf_mod %>%
      dplyr::filter(
        SOURCE_TRACTID %in% half2_tract_list,
        NEIGHBOR_TRACTID %in% half2_tract_list
      )
    
    # pull list of enclaves
    enclave_list_half_a <- identifyEnclaves(half1_tract_list, adj_half_a)
    enclave_list_half_b <- identifyEnclaves(half2_tract_list, adj_half_b)
    
  }
  
  ##### STEP 6: ADD ENCLAVE DATA
  ##### Split the enclaves out from their surrounding tract.
  
  # pull tracts for tracts surrounding enclaves, to split back into the
  # surrounding tract and its enclave(s)
  tdf_surrounding_to_add <- tdf %>%
    dplyr::filter(Geography %in% surrounding_list) %>%
    dplyr::rename(Geography_combined = Geography) %>%
    dplyr::select(-Population) %>%
    dplyr::left_join(df,
                     dplyr::join_by("Geography_combined" == "Geography")) %>%
    dplyr::rename(Geography = Geography_combined)
  
  # pull population data for enclaves and assign the enclaves the districts
  # of their respective surrounding tracts
  tdf_enclave_to_add <- df %>%
    dplyr::filter(Geography %in% enclaves_list) %>%
    dplyr::left_join(master_enclave_data,
                     dplyr::join_by("Geography" == "ENCLAVE_TRACTID")) %>%
    dplyr::left_join(tdf_surrounding_to_add %>%
                       dplyr::select(-Population),
                     dplyr::join_by("SURROUNDING_TRACTID" == "Geography")) %>%
    dplyr::select(-SURROUNDING_TRACTID)
  
  # remove combined districts and append with the separated enclave and
  # surrounding district data
  tdf <- tdf %>%
    dplyr::filter(Geography %!in% c(enclaves_list, surrounding_list)) %>%
    rbind(tdf_enclave_to_add, tdf_surrounding_to_add) %>%
    as.data.frame()
  
  ##### STEP 7: FINALIZE DATA
  ##### Pull the final data outputs and test each half for continuity.
  
  # pull data for Side A
  half1_tract_list <- tdf %>%
    dplyr::filter(half == 1) %>%
    dplyr::pull(Geography) %>%
    unique()
  
  # pull data for Side B
  half2_tract_list <- tdf %>%
    dplyr::filter(half == 2) %>%
    dplyr::pull(Geography) %>%
    unique()
  
  # create new population sets for Side A...
  half1_population <- df %>%
    dplyr::filter(Geography %in% half1_tract_list)
  # ...and Side B
  half2_population <- df %>%
    dplyr::filter(Geography %in% half2_tract_list)
  
  # create new adjacency matrices for Side A...
  adj_half1 <- adjdf %>%
    dplyr::filter(SOURCE_TRACTID %in% half1_tract_list,
                  NEIGHBOR_TRACTID %in% half1_tract_list
    )
  # ...and Side B
  adj_half2 <- adjdf %>%
    dplyr::filter(SOURCE_TRACTID %in% half2_tract_list,
                  NEIGHBOR_TRACTID %in% half2_tract_list
    )
  
  # test if Side A and Side B are both contiguous
  sideA_contig <- isContig(half1_tract_list,adj_half1)
  sideB_contig <- isContig(half2_tract_list,adj_half2)
  
  output <- list(half1_population, half2_population, adj_half1, adj_half2)
  
  if(sideA_contig == 1 & sideB_contig == 1){
    return(output)
  }
  if(sideA_contig == 0 | sideB_contig == 0){
    warning(paste("Error: Function produced a non-contiguous district."))
    return(output)
  }
  
}


### Split 1: Two Parts ----------------------------------------------------------------------
ver <- 1

start_time <- Sys.time()
split1 <- splitIntoTwo(pop_tracts_total, adjacency_list, version = ver)
end_time <- Sys.time()
end_time - start_time

# split1 <- splitIntoTwo(pop_tracts_total_v2, adjacency_list_v2, version = ver)

# maptest <- split1[[1]] %>%
#   dplyr::full_join(shape_tract, by = "Geography") %>%
#   dplyr::filter(!is.na(Population)) %>%
#   sf::st_as_sf()
# mapview(maptest)

pop_half1 <- split1[[1]]
pop_half2 <- split1[[2]]

adj_half1 <- split1[[3]]
adj_half2 <- split1[[4]]

# population totals for each half
sum(pop_half1$Population, na.rm = TRUE)
sum(pop_half2$Population, na.rm = TRUE)


### Split 2: Four Parts ----------------------------------------------------------------------

#### Split 2A --------------------------------------------------------------------------------
# Split Half1 into Quarter1 and Quarter2

start_time <- Sys.time()
split2a <- splitIntoTwo(pop_half1, adj_half1, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_quarter1 <- split2a[[1]]
pop_quarter2 <- split2a[[2]]

adj_quarter1 <- split2a[[3]]
adj_quarter2 <- split2a[[4]]

#### Split 2B --------------------------------------------------------------------------------
# Split Half2 into Quarter3 and Quarter4

start_time <- Sys.time()
split2b <- splitIntoTwo(pop_half2, adj_half2, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_quarter3 <- split2b[[1]]
pop_quarter4 <- split2b[[2]]

adj_quarter3 <- split2b[[3]]
adj_quarter4 <- split2b[[4]]

#### Split 2 Populations --------------------------------------------------------------------------------

# population totals for each quarter
sum(pop_quarter1$Population, na.rm = TRUE)
sum(pop_quarter2$Population, na.rm = TRUE)
sum(pop_quarter3$Population, na.rm = TRUE)
sum(pop_quarter4$Population, na.rm = TRUE)

# test if the minimum population value is in the 99.5% - 100.5% of mean target range
quarter_min_ratio <- min(
  sum(pop_quarter1$Population, na.rm = TRUE), sum(pop_quarter2$Population, na.rm = TRUE),
  sum(pop_quarter3$Population, na.rm = TRUE), sum(pop_quarter4$Population, na.rm = TRUE)) /
  (sum(pop_tracts_total$Population, na.rm = TRUE) / 4) 
quarter_min_ratio

quarter_min_ratio < 0.995


### Split 3: Eight Parts ----------------------------------------------------------------------

#### Split 3A --------------------------------------------------------------------------------
# Split Quarter1 into Eigth1 and Eigth2

start_time <- Sys.time()
split3a <- splitIntoTwo(pop_quarter1, adj_quarter1, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_eigth1 <- split3a[[1]]
pop_eigth2 <- split3a[[2]]

adj_eigth1 <- split3a[[3]]
adj_eigth2 <- split3a[[4]]

#### Split 3B --------------------------------------------------------------------------------
# Split Quarter2 into Eigth3 and Eigth4

start_time <- Sys.time()
split3b <- splitIntoTwo(pop_quarter2, adj_quarter2, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_eigth3 <- split3b[[1]]
pop_eigth4 <- split3b[[2]]

adj_eigth3 <- split3b[[3]]
adj_eigth4 <- split3b[[4]]

#### Split 3C --------------------------------------------------------------------------------
# Split Quarter3 into Eigth5 and Eigth6

start_time <- Sys.time()
split3c <- splitIntoTwo(pop_quarter3, adj_quarter3, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_eigth5 <- split3c[[1]]
pop_eigth6 <- split3c[[2]]

adj_eigth5 <- split3c[[3]]
adj_eigth6 <- split3c[[4]]

#### Split 3D --------------------------------------------------------------------------------
# Split Quarter4 into Eigth7 and Eigth8

start_time <- Sys.time()
split3d <- splitIntoTwo(pop_quarter4, adj_quarter4, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_eigth7 <- split3d[[1]]
pop_eigth8 <- split3d[[2]]

adj_eigth7 <- split3d[[3]]
adj_eigth8 <- split3d[[4]]

#### Split 3 Populations --------------------------------------------------------------------------------

# population totals for each quarter
sum(pop_eigth1$Population, na.rm = TRUE)
sum(pop_eigth2$Population, na.rm = TRUE)
sum(pop_eigth3$Population, na.rm = TRUE)
sum(pop_eigth4$Population, na.rm = TRUE)
sum(pop_eigth5$Population, na.rm = TRUE)
sum(pop_eigth6$Population, na.rm = TRUE)
sum(pop_eigth7$Population, na.rm = TRUE)
sum(pop_eigth8$Population, na.rm = TRUE)


### Split 4: Sixteen Parts ----------------------------------------------------------------------

#### Split 4A --------------------------------------------------------------------------------
# Split Eigth1 into Sixteenth1 and Sixteenth2

start_time <- Sys.time()
split4a <- splitIntoTwo(pop_eigth1, adj_eigth1, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_sixteenth1 <- split4a[[1]]
pop_sixteenth2 <- split4a[[2]]

adj_sixteenth1 <- split4a[[3]]
adj_sixteenth2 <- split4a[[4]]

#### Split 4B --------------------------------------------------------------------------------
# Split Eigth2 into Sixteenth3 and Sixteenth4

start_time <- Sys.time()
split4b <- splitIntoTwo(pop_eigth2, adj_eigth2, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_sixteenth3 <- split4b[[1]]
pop_sixteenth4 <- split4b[[2]]

adj_sixteenth3 <- split4b[[3]]
adj_sixteenth4 <- split4b[[4]]

#### Split 4C --------------------------------------------------------------------------------
# Split Eigth3 into Sixteenth5 and Sixteenth6

start_time <- Sys.time()
split4c <- splitIntoTwo(pop_eigth3, adj_eigth3, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_sixteenth5 <- split4c[[1]]
pop_sixteenth6 <- split4c[[2]]

adj_sixteenth5 <- split4c[[3]]
adj_sixteenth6 <- split4c[[4]]

#### Split 4D --------------------------------------------------------------------------------
# Split Eigth4 into Sixteenth7 and Sixteenth8

start_time <- Sys.time()
split4d <- splitIntoTwo(pop_eigth4, adj_eigth4, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_sixteenth7 <- split4d[[1]]
pop_sixteenth8 <- split4d[[2]]

adj_sixteenth7 <- split4d[[3]]
adj_sixteenth8 <- split4d[[4]]

#### Split 4E --------------------------------------------------------------------------------
# Split Eigth5 into Sixteenth9 and Sixteenth10

start_time <- Sys.time()
split4e <- splitIntoTwo(pop_eigth5, adj_eigth5, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_sixteenth9 <- split4e[[1]]
pop_sixteenth10 <- split4e[[2]]

adj_sixteenth9 <- split4e[[3]]
adj_sixteenth10 <- split4e[[4]]

#### Split 4F --------------------------------------------------------------------------------
# Split Eigth6 into Sixteenth11 and Sixteenth12

start_time <- Sys.time()
split4f <- splitIntoTwo(pop_eigth6, adj_eigth6, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_sixteenth11 <- split4f[[1]]
pop_sixteenth12 <- split4f[[2]]

adj_sixteenth11 <- split4f[[3]]
adj_sixteenth12 <- split4f[[4]]

#### Split 4G --------------------------------------------------------------------------------
# Split Eigth7 into Sixteenth13 and Sixteenth14

start_time <- Sys.time()
split4g <- splitIntoTwo(pop_eigth7, adj_eigth7, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_sixteenth13 <- split4g[[1]]
pop_sixteenth14 <- split4g[[2]]

adj_sixteenth13 <- split4g[[3]]
adj_sixteenth14 <- split4g[[4]]

#### Split 4H --------------------------------------------------------------------------------
# Split Eigth8 into Sixteenth15 and Sixteenth16

start_time <- Sys.time()
split4h <- splitIntoTwo(pop_eigth8, adj_eigth8, version = ver)
end_time <- Sys.time()
end_time - start_time

pop_sixteenth15 <- split4h[[1]]
pop_sixteenth16 <- split4h[[2]]

adj_sixteenth15 <- split4h[[3]]
adj_sixteenth16 <- split4h[[4]]

#### Split 4 Populations --------------------------------------------------------------------------------

# population totals for each quarter
sum(pop_sixteenth1$Population, na.rm = TRUE)
sum(pop_sixteenth2$Population, na.rm = TRUE)
sum(pop_sixteenth3$Population, na.rm = TRUE)
sum(pop_sixteenth4$Population, na.rm = TRUE)
sum(pop_sixteenth5$Population, na.rm = TRUE)
sum(pop_sixteenth6$Population, na.rm = TRUE)
sum(pop_sixteenth7$Population, na.rm = TRUE)
sum(pop_sixteenth8$Population, na.rm = TRUE)
sum(pop_sixteenth9$Population, na.rm = TRUE)
sum(pop_sixteenth10$Population, na.rm = TRUE)
sum(pop_sixteenth11$Population, na.rm = TRUE)
sum(pop_sixteenth12$Population, na.rm = TRUE)
sum(pop_sixteenth13$Population, na.rm = TRUE)
sum(pop_sixteenth14$Population, na.rm = TRUE)
sum(pop_sixteenth15$Population, na.rm = TRUE)
sum(pop_sixteenth16$Population, na.rm = TRUE)

min(sum(pop_sixteenth1$Population, na.rm = TRUE),
  sum(pop_sixteenth2$Population, na.rm = TRUE),
  sum(pop_sixteenth3$Population, na.rm = TRUE),
  sum(pop_sixteenth4$Population, na.rm = TRUE),
  sum(pop_sixteenth5$Population, na.rm = TRUE),
  sum(pop_sixteenth6$Population, na.rm = TRUE),
  sum(pop_sixteenth7$Population, na.rm = TRUE),
  sum(pop_sixteenth8$Population, na.rm = TRUE),
  sum(pop_sixteenth9$Population, na.rm = TRUE),
  sum(pop_sixteenth10$Population, na.rm = TRUE),
  sum(pop_sixteenth11$Population, na.rm = TRUE),
  sum(pop_sixteenth12$Population, na.rm = TRUE),
  sum(pop_sixteenth13$Population, na.rm = TRUE),
  sum(pop_sixteenth14$Population, na.rm = TRUE),
  sum(pop_sixteenth15$Population, na.rm = TRUE),
  sum(pop_sixteenth16$Population, na.rm = TRUE)) /
  (sum(pop_tracts_total$Population, na.rm = TRUE) / 16)

### Create Final Dataset ----------------------------------------------------------------------

pop_final <- pop_tracts_total %>%
  dplyr::mutate(
    district = dplyr::case_when(
      Geography %in% pop_sixteenth1$Geography ~ 1,
      Geography %in% pop_sixteenth2$Geography ~ 2,
      Geography %in% pop_sixteenth3$Geography ~ 3,
      Geography %in% pop_sixteenth4$Geography ~ 4,
      Geography %in% pop_sixteenth5$Geography ~ 5,
      Geography %in% pop_sixteenth6$Geography ~ 6,
      Geography %in% pop_sixteenth7$Geography ~ 7,
      Geography %in% pop_sixteenth8$Geography ~ 8,
      Geography %in% pop_sixteenth9$Geography ~ 9,
      Geography %in% pop_sixteenth10$Geography ~ 10,
      Geography %in% pop_sixteenth11$Geography ~ 11,
      Geography %in% pop_sixteenth12$Geography ~ 12,
      Geography %in% pop_sixteenth13$Geography ~ 13,
      Geography %in% pop_sixteenth14$Geography ~ 14,
      Geography %in% pop_sixteenth15$Geography ~ 15,
      Geography %in% pop_sixteenth16$Geography ~ 16,
      .default = NA
    )
  )

# table(pop_final$district, useNA = "always")

write.csv(pop_final, "Tracts 2010 (alg2)/Export Data/District Outputs Tracts 2010/output20.csv", row.names = FALSE)


### Create Map -----------------------------------------------------------------------

color_palette <- c("dodgerblue3","firebrick2","chartreuse2","darkorchid3","darkslategray4",
                   "orange2","lightsalmon","hotpink1","turquoise","darkseagreen2","lightblue3",
                   "mediumorchid1","plum","ivory2","goldenrod","olivedrab3")

map <- pop_final %>%
  dplyr::left_join(shape_tract,
                   by = "Geography")

for(a in c(1:16)){
  
  tmp <- map %>%
    dplyr::filter(district == a) %>%
    sf::st_as_sf() %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(district = a, .before = 1) %>%
    dplyr::rename(geometry = 2)
  
  assign(paste0("district_shape_",a),tmp)
  
}

district_map_shapefile <- mget(ls(pattern="district_shape_")) %>%
  bind_rows() %>%
  dplyr::mutate(District = factor(district, c(1:16))) %>%
  dplyr::select(-district)

map <- ggplot2::ggplot(data = district_map_shapefile,
                       ggplot2::aes(fill = District)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_manual(values = color_palette) +
  ggplot2::theme_void()

map


# map using mapview function
map_mapview <- pop_final %>%
  dplyr::left_join(shape_tract,
                   by = "Geography") %>%
  sf::st_as_sf()

mapview(map_mapview, zcol = "district", col.regions = mapviewPalette("mapviewSpectralColors"))



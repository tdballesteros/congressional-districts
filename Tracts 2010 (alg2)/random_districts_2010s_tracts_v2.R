
# This file computes Ohio's 16 congressional districts using the 2010 US Census data and 2020
# election results, which used districts drawn from the 2010 Census data.

# v2 changes:
## Adjacency matrix is calculated from the US Census Bureau / TIGER/Line shapefile directly and
## no longer uses queen's adjacency (overlap must be at more than one point)


### load libraries ----------------------------------------------------------------------
library(readxl)
library(sf)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

## Adjacency Matrix/List
# Source: Derived from US Census Bureau and TIGER/Line
# adjacency_matrix <- read.csv("Data/Calculated_Adjacency_Matrix_Tracts_2010.csv",
#                              colClasses = "character")
adjacency_list <- read.csv("Data/Calculated_Adjacency_List_Tracts_2010.csv",
                           colClasses = "character")

# v2 removes six unpopulated lake tracts
# adjacency_matrix_v2 <- read.csv("Data/Calculated_Adjacency_Matrix_v2_Tracts_2010.csv",
#                                 colClasses = "character")
adjacency_list_v2 <- read.csv("Data/Calculated_Adjacency_List_v2_Tracts_2010.csv",
                              colClasses = "character")

## Population Data
# Source: US Census Bureau
population_data <- read.csv("Data/population_data_2010_by_tract.csv",
                            skip = 1,
                            colClasses = "character")

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
  # several tracts have non-numeric entries in the Population  field; manually
  # correct these and convert all data to numeric values
  dplyr::mutate(
    Population = dplyr::case_when(
      Geography == "1400000US39035118800" ~ 3081,
      Geography == "1400000US39035141300" ~ 2661,
      Geography == "1400000US39035187105" ~ 2176,
      Geography == "1400000US39035187106" ~ 5198,
      Geography == "1400000US39035195900" ~ 4233,
      Geography == "1400000US39055310200" ~ 1991,
      Geography == "1400000US39055310600" ~ 6148,
      Geography == "1400000US39055310800" ~ 6621,
      Geography == "1400000US39055310900" ~ 3200,
      Geography == "1400000US39055311000" ~ 3637,
      Geography == "1400000US39055311300" ~ 4412,
      Geography == "1400000US39055311400" ~ 5537,
      Geography == "1400000US39055311600" ~ 3810,
      Geography == "1400000US39055311700" ~ 4089,
      Geography == "1400000US39055311800" ~ 7306,
      Geography == "1400000US39055312100" ~ 4131,
      Geography == "1400000US39055312202" ~ 4323,
      Geography == "1400000US39055312300" ~ 4643,
      Geography == "1400000US39055312400" ~ 2544,
      Geography == "1400000US39085206400" ~ 4701,
      Geography == "1400000US39155930500" ~ 6115,
      .default = as.numeric(Population)
    )) %>%
  dplyr::mutate(
    Geography = stringr::str_sub(Geography, 10)
  ) %>%
  # select columns
  dplyr::select(Geography, Population)


# create modified population data by merging tract enclaves with their surrounding tract,
# as these tracts will always need to be within the same district

# SOURCE_TRACTID is the enclave, NEIGHBOR_TRACTID is the surrounding tract
tract_enclave_df <- adjacency_list %>%
  dplyr::group_by(SOURCE_TRACTID) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == 1) %>%
  dplyr::select(-n) %>%
  dplyr::left_join(adjacency_list, by = "SOURCE_TRACTID")

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

tract_enclave_list <- tract_enclave_df$ENCLAVE_TRACTID
tract_enclave_list_v2 <- tract_enclave_df_v2$ENCLAVE_TRACTID

tract_surrounding_list <- tract_enclave_df$SURROUNDING_TRACTID
tract_surrounding_list_v2 <- tract_enclave_df_v2$SURROUNDING_TRACTID

# create modified population dataset with enclaves merged into their surrounding tracts' population
pop_tracts_total_mod <- pop_tracts_total %>%
  dplyr::full_join(tract_enclave_df,
                   dplyr::join_by("Geography" == "ENCLAVE_TRACTID")) %>%
  dplyr::mutate(Geography = dplyr::coalesce(SURROUNDING_TRACTID, Geography)) %>%
  dplyr::select(-SURROUNDING_TRACTID) %>%
  dplyr::group_by(Geography) %>%
  dplyr::summarise(Population = sum(Population, na.rm = TRUE)) %>%
  dplyr::ungroup()

pop_tracts_total_mod_v2 <- pop_tracts_total %>%
  dplyr::full_join(tract_enclave_df_v2,
                   dplyr::join_by("Geography" == "ENCLAVE_TRACTID")) %>%
  dplyr::mutate(Geography = dplyr::coalesce(SURROUNDING_TRACTID, Geography)) %>%
  dplyr::select(-SURROUNDING_TRACTID) %>%
  dplyr::group_by(Geography) %>%
  dplyr::summarise(Population = sum(Population, na.rm = TRUE)) %>%
  dplyr::ungroup()

# create modified adjacency matrices/lists without enclaves
# adjacency_matrix_mod <- adjacency_matrix %>%
#   dplyr::select(-tract_enclave_df$ENCLAVE_TRACTID) %>%
#   dplyr::filter(SOURCE_TRACTID %!in% tract_enclave_df$ENCLAVE_TRACTID)

adjacency_list_mod <- adjacency_list %>%
  dplyr::filter(
    SOURCE_TRACTID %!in% tract_enclave_df$ENCLAVE_TRACTID,
    NEIGHBOR_TRACTID %!in% tract_enclave_df$ENCLAVE_TRACTID
    )

# adjacency_matrix_mod_v2 <- adjacency_matrix_v2 %>%
#   dplyr::select(-tract_enclave_df_v2$ENCLAVE_TRACTID) %>%
#   dplyr::filter(SOURCE_TRACTID %!in% tract_enclave_df_v2$ENCLAVE_TRACTID)

adjacency_list_mod_v2 <- adjacency_list_v2 %>%
  dplyr::filter(
    SOURCE_TRACTID %!in% tract_enclave_df_v2$ENCLAVE_TRACTID,
    NEIGHBOR_TRACTID %!in% tract_enclave_df_v2$ENCLAVE_TRACTID
  )


### functions ----------------------------------------------------------------------

tractdist <- function(tracts, adjdf = adjacency_list, popdf = pop_tracts_total){
  
  # test for contiguousness
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
isContig <- function(list, adjdf = adj){
  
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


splitIntoTwo <- function(df = pop_tracts_total, adjdf = adjacency_list, version = 1){
  
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
  
  # set initial cumulative sum ("cmsm") at 0
  cmsm <- 0
  
  # calculate the sum goal
  sum_goal <- (sum(tdf$Population, na.rm = TRUE) / 2) - median(tdf$Population, na.rm = TRUE)
  
  # calculate how close to equidistant the closest to equidistant tracts are
  # a value of 0 is equidistant
  j <- min(tdf$distfrom, na.rm = TRUE)
  
  # while the cumulative sum is <= the population goal...
  while(cmsm <= sum_goal){
    
    # filter for the distance being closer to equidistant
    tmp <- tdf %>%
      dplyr::filter(distfrom <= j)
    
    # calculate cumulative sum of radius size j
    cmsm <- sum(tmp$Population, na.rm = TRUE)
    
    # increase radius
    j <- j + 1
  }
  
  # determine middle distance between tract1 and tract2 and pull
  # tracts here
  split_list <- tdf %>%
    dplyr::filter(distfrom == j-1) %>%
    dplyr::pull(Geography)
  
  # calculate all tracts solidly on half "A"
  half_a <- tdf %>%
    dplyr::filter(distfrom < j-1)
  
  # Half "A" population estimate
  pop_half_a <- sum(half_a$Population)
  alpha <- unique(half_a$Geography)
  
  # calculate all tracts solidly on half "B"
  half_b <- tdf %>%
    dplyr::filter(distfrom >= j)
  
  # Half "B" population estimates
  pop_half_b <- sum(half_b$Population)
  beta <- unique(half_b$Geography)
  
  # create a df that has the midpoint tracts listed
  whichside <- data.frame(Geography = split_list) %>%
    dplyr::rowwise() %>%
    # for each tract, calculate how many neighboring tracts it has
    # in the adjacency matrix that are in alpha/beta lists, respectively
    dplyr::mutate(
      alif = adjdf %>%
        dplyr::filter(
          SOURCE_TRACTID == as.character(Geography),
          NEIGHBOR_TRACTID %in% alpha) %>%
        unique() %>%
        nrow(),
      ba = adjdf %>%
        dplyr::filter(
          SOURCE_TRACTID == as.character(Geography),
          NEIGHBOR_TRACTID %in% beta) %>%
        unique() %>%
        nrow()
      )
  
  # while the population is <= the population target...
  while(pop_half_a <= sum_goal){
    
    # pull the "middle" tracts and calculate how many more neighboring
    # tracts are on Side A/Side B
    ## positive - Side B has more neighbors
    ## negative - Side A has more neighbors
    temp <- whichside %>%
      dplyr::mutate(side = ba - alif) %>%
      # sort by number of adjacent neighbors with more neighbors
      # in Side A at the top
      dplyr::arrange(side)
    
    # set second cumulative sum (cmsm2) to 0
    cmsm2 <- 0
    q <- 1
    
    # calculate the difference in population to make up between Side A and
    # the original population goal
    sum_goal2 <- sum_goal - pop_half_a - median(tdf$Population, na.rm = TRUE)
    
    # while cmsm2 is <= sum_goal2...
    while(cmsm2 <= sum_goal2){
      
      # pull the q "middle" tracts with the most neighbors in Side A
      tmp <- temp[c(1:q),]
      
      # pull tracts
      tmp_list <- tmp$Geography
      
      # select these tracts
      tmp_df <- tdf %>%
        dplyr::filter(Geography %in% tmp_list)
      
      # calculate the second cumulative sum to see if that brings
      # Side A to the goal
      cmsm2 <- sum(tmp_df$Population, na.rm = TRUE)
      
      # increase the counter
      q = q + 1
    }
    
    # merge together Side A data
    half_a <- rbind(half_a, tmp_df)
    
    # list of Side A tracts
    half_a_list <- unique(half_a$Geography)
    
    # calculate Side A population
    pop_half_a <- sum(half_a$Population, na.rm = TRUE)
  }
  
  # in the original df, code tracts as within Side A
  # or Side B
  tdf <- tdf %>%
    dplyr::rowwise() %>%
    dplyr::mutate(half = dplyr::case_when(
      Geography %in% half_a_list ~ 1,
      .default = 2
    ))
  
  # pull tracts for tracts surrounding enclaves, to split back into the
  # surrounding tract and its enclave(s)
  tdf_surrounding <- tdf %>%
    dplyr::filter(Geography %in% surrounding_list) %>%
    dplyr::rename(Geography_combined = Geography)
  
  # reverted combined tract populations back to just the tract's population
  tdf_surrounding_to_add <- tdf_surrounding %>%
    dplyr::select(-Population) %>%
    dplyr::left_join(df,
                     dplyr::join_by("Geography_combined" == "Geography")) %>%
    dplyr::rename(Geography = Geography_combined)
  
  # pull population data for enclaves and assign the enclaves the districts
  # of their respective surrounding tracts
  tdf_enclave_to_add <- df %>%
    dplyr::filter(Geography %in% enclaves_list) %>%
    dplyr::left_join(tract_enclave_df,
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
 
  half1_list_tmp <- tdf %>%
    dplyr::filter(half == 1) %>%
    dplyr::pull(Geography)
  
  half2_list_tmp <- tdf %>%
    dplyr::filter(half == 2) %>%
    dplyr::pull(Geography)
  
  # pull number of adjacent tracts and tracts in each half to determine non-contiguousness
  tdf <- tdf %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_adj = adjdf %>%
        dplyr::filter(SOURCE_TRACTID == Geography) %>%
        nrow(),
      half1_adj = adjdf %>%
        dplyr::filter(
          SOURCE_TRACTID == Geography,
          NEIGHBOR_TRACTID %in% half1_list_tmp
        ) %>%
        nrow(),
      half2_adj = adjdf %>%
        dplyr::filter(
          SOURCE_TRACTID == Geography,
          NEIGHBOR_TRACTID %in% half2_list_tmp
        ) %>%
        nrow(),
      half1_adj_within = dplyr::case_when(
        half == 1 ~ half1_adj,
        .default = NA
      ),
      half2_adj_within = dplyr::case_when(
        half == 2 ~ half2_adj,
        .default = NA
      ),
      half_adj_within = dplyr::coalesce(half1_adj_within, half2_adj_within),
      # if there are no adjacent tracts within the same half, flip the half
      half = dplyr::case_when(
        half == 1 & half_adj_within == 0 ~ 2,
        half == 2 & half_adj_within == 0 ~ 1,
        .default = half
      ))
  
  # pull data for Side A
  half1_list <- tdf %>%
    dplyr::filter(half == 1) %>%
    dplyr::pull(Geography) %>%
    unique()
  
  # pull data for Side B
  half2_list <- tdf %>%
    dplyr::filter(half == 2) %>%
    dplyr::pull(Geography) %>%
    unique()
  
  # create new population sets for Side A...
  pop_half1 <- df %>%
    dplyr::filter(Geography %in% half1_list)
  # ...and Side B
  pop_half2 <- df %>%
    dplyr::filter(Geography %in% half2_list)
  
  # create new adjacency matrices for Side A...
  adj_half1 <- adjdf %>%
    dplyr::filter(SOURCE_TRACTID %in% half1_list,
                  NEIGHBOR_TRACTID %in% half1_list
    )
  # ...and Side B
  adj_half2 <- adjdf %>%
    dplyr::filter(SOURCE_TRACTID %in% half2_list,
                  NEIGHBOR_TRACTID %in% half2_list
    )
  
  # test if Side A and Side B are both contiguous
  sideA_contig <- isContig(half1_list,adj_half1)
  sideB_contig <- isContig(half2_list,adj_half2)
  
  output <- list(pop_half1, pop_half2, adj_half1, adj_half2)
  
  if(sideA_contig == 1 & sideB_contig == 1){
    return(output)
  }
  if(sideA_contig == 0 | sideB_contig == 0){
    warning(paste("Error: Function produced a non-contiguous district."))
    return(output)
  }
  
}

### Split 1: Two Parts ----------------------------------------------------------------------

split1 <- splitIntoTwo(pop_tracts_total, adjacency_list, version = 1)

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

split2a <- splitIntoTwo(pop_half1, adj_half1, version = 1)
pop_quarter1 <- split2a[[1]]
pop_quarter2 <- split2a[[2]]

adj_quarter1 <- split2a[[3]]
adj_quarter2 <- split2a[[4]]

#### Split 2B --------------------------------------------------------------------------------
# Split Half2 into Quarter3 and Quarter4

split2b <- splitIntoTwo(pop_half2, adj_half2, version = 1)
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


### Split 3: Eight Parts ----------------------------------------------------------------------

#### Split 3A --------------------------------------------------------------------------------
# Split Quarter1 into Eigth1 and Eigth2

split3a <- splitIntoTwo(pop_quarter1, adj_quarter1, version = 1)
pop_eigth1 <- split3a[[1]]
pop_eigth2 <- split3a[[2]]

adj_eigth1 <- split3a[[3]]
adj_eigth2 <- split3a[[4]]

#### Split 3B --------------------------------------------------------------------------------
# Split Quarter2 into Eigth3 and Eigth4

split3b <- splitIntoTwo(pop_quarter2, adj_quarter2, version = 1)
pop_eigth3 <- split3b[[1]]
pop_eigth4 <- split3b[[2]]

adj_eigth3 <- split3b[[3]]
adj_eigth4 <- split3b[[4]]

#### Split 3C --------------------------------------------------------------------------------
# Split Quarter3 into Eigth5 and Eigth6

split3c <- splitIntoTwo(pop_quarter3, adj_quarter3, version = 1)
pop_eigth5 <- split3c[[1]]
pop_eigth6 <- split3c[[2]]

adj_eigth5 <- split3c[[3]]
adj_eigth6 <- split3c[[4]]

#### Split 3D --------------------------------------------------------------------------------
# Split Quarter4 into Eigth7 and Eigth8

split3d <- splitIntoTwo(pop_quarter4, adj_quarter4, version = 1)
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

split4a <- splitIntoTwo(pop_eigth1, adj_eigth1, version = 1)
pop_sixteenth1 <- split4a[[1]]
pop_sixteenth2 <- split4a[[2]]

adj_sixteenth1 <- split4a[[3]]
adj_sixteenth2 <- split4a[[4]]

#### Split 4B --------------------------------------------------------------------------------
# Split Eigth2 into Sixteenth3 and Sixteenth4

split4b <- splitIntoTwo(pop_eigth2, adj_eigth2, version = 1)
pop_sixteenth3 <- split4b[[1]]
pop_sixteenth4 <- split4b[[2]]

adj_sixteenth3 <- split4b[[3]]
adj_sixteenth4 <- split4b[[4]]

#### Split 4C --------------------------------------------------------------------------------
# Split Eigth3 into Sixteenth5 and Sixteenth6

split4c <- splitIntoTwo(pop_eigth3, adj_eigth3, version = 1)
pop_sixteenth5 <- split4c[[1]]
pop_sixteenth6 <- split4c[[2]]

adj_sixteenth5 <- split4c[[3]]
adj_sixteenth6 <- split4c[[4]]

#### Split 4D --------------------------------------------------------------------------------
# Split Eigth4 into Sixteenth7 and Sixteenth8

split4d <- splitIntoTwo(pop_eigth4, adj_eigth4, version = 1)
pop_sixteenth7 <- split4d[[1]]
pop_sixteenth8 <- split4d[[2]]

adj_sixteenth7 <- split4d[[3]]
adj_sixteenth8 <- split4d[[4]]

#### Split 4E --------------------------------------------------------------------------------
# Split Eigth5 into Sixteenth9 and Sixteenth10

split4e <- splitIntoTwo(pop_eigth5, adj_eigth5, version = 1)
pop_sixteenth9 <- split4e[[1]]
pop_sixteenth10 <- split4e[[2]]

adj_sixteenth9 <- split4e[[3]]
adj_sixteenth10 <- split4e[[4]]

#### Split 4F --------------------------------------------------------------------------------
# Split Eigth6 into Sixteenth11 and Sixteenth12

split4f <- splitIntoTwo(pop_eigth6, adj_eigth6, version = 1)
pop_sixteenth11 <- split4f[[1]]
pop_sixteenth12 <- split4f[[2]]

adj_sixteenth11 <- split4f[[3]]
adj_sixteenth12 <- split4f[[4]]

#### Split 4G --------------------------------------------------------------------------------
# Split Eigth7 into Sixteenth13 and Sixteenth14

split4g <- splitIntoTwo(pop_eigth7, adj_eigth7, version = 1)
pop_sixteenth13 <- split4g[[1]]
pop_sixteenth14 <- split4g[[2]]

adj_sixteenth13 <- split4g[[3]]
adj_sixteenth14 <- split4g[[4]]

#### Split 4H --------------------------------------------------------------------------------
# Split Eigth8 into Sixteenth15 and Sixteenth16

split4h <- splitIntoTwo(pop_eigth8, adj_eigth8, version = 1)
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

# write.csv(pop_final, "District Outputs Tracts 2010/output30.csv", row.names = FALSE)


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

maps


# map using mapview function
map_mapview <- pop_final %>%
  dplyr::left_join(ohio2010shapefile,
                   by = "Geography") %>%
  sf::st_as_sf()

mapview(map_mapview, zcol = "district", col.regions = mapviewPalette("mapviewSpectralColors"))



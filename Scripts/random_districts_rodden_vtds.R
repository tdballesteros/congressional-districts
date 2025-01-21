
# This script uses Ohio's voting districts to construct 16 national Congressional districts using
# 2010 US Census population data at the voting district level.

# Rodden: https://web.stanford.edu/~jrodden/OH_release/

### load libraries ----------------------------------------------------------------------

library(readxl)
library(sf)
library(ggplot2)
library(mapview)
library(tidyverse)

### 'not in' function ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------

# Voting District Adjacency Data 2010
## Source: Derived from US Census Bureau and TIGER/Line shapefiles
adj <- read.csv("Data/adjacency_list_vtds_2010.csv",
                colClasses = "character")


# Ohio 2010 Demographic Data by Voting District
## Source: US Census Bureau
oh2010 <- read.csv("Data/population_data_2010_by_vtd.csv",
                   skip = 1)


# Ohio 2008 Voting Data by Voting District
## Source: Rodden, SDA, and US Census Bureau
vtd_data <- read.csv("Data/oh_vote_merged_final.csv")


## Map VTD Shapefiles
# Source: US Census Bureau and TIGER/Line
area <- sf::read_sf(
  dsn = "Data/shapefile_ohio_vtd_2010.shp") %>%
  dplyr::rename_with(tolower)

### format data ----------------------------------------------------------------------

vtd_data <- vtd_data %>%
  # recode a typo
  dplyr::mutate(GEOID10 = ifelse(GEOID10 == "3906935AAH", "39069035AAH", GEOID10))

adj <- adj %>%
  # recode a typo
  dplyr::mutate(
    SOURCE_VTDID = ifelse(SOURCE_VTDID == "3906935AAH", "39069035AAH", SOURCE_VTDID),
    NEIGHBOR_VTDID = ifelse(NEIGHBOR_VTDID == "3906935AAH", "39069035AAH", NEIGHBOR_VTDID)
  )

pop <- oh2010 %>%
  dplyr::mutate(
    GEOID10 = stringr::str_sub(Geography, 10)
  ) %>%
  dplyr::full_join(vtd_data, by = "GEOID10") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    rmissing = is.na(total_08),
    omissing = is.na(Total)
  )

missing_table <- pop %>%
  dplyr::filter(rmissing == TRUE | omissing == TRUE) %>%
  dplyr::select(GEOID10, Geography, Geographic.Area.Name, Total, oh_vote_order:precinct_code, COUNTY_NUMBER:PRECINCT_NAME, total_08, precid_08:omissing) %>%
  dplyr::mutate(
    ocounty = stringr::str_sub(Geography, 12, 14),
    otract = stringr::str_sub(Geography, 15),
    rcounty = stringr::str_pad(COUNTY_NUMBER, width = 3, pad = "0", side ="left"),
    county_coa = dplyr::coalesce(ocounty,rcounty)
  )

pop_small <- pop %>%
  dplyr::select(GEOID10, Geography, Geographic.Area.Name, Total, oh_vote_order:precinct_code, COUNTY_NUMBER:PRECINCT_NAME, total_08, precid_08:omissing)

table(pop$rmissing, useNA = "always") # 23 missing, 11107 not missing
sum(missing_table$Total, na.rm = TRUE) # population with unmatched voting records - 11,772

table(pop$omissing, useNA = "always") # 101 missing, 11029 not missing
sum(missing_table$total_08, na.rm = TRUE) # voting records with unmatched population - 43,736

pop <- pop %>%
  dplyr::select(GEOID10, Total) %>%
  na.omit() %>%
  dplyr::rename(
    GEO_ID = GEOID10,
    CIT_EST = Total
  )

pop_vtd <- unique(pop$GEO_ID)
adj_vtd <- unique(adj$SOURCE_VTDID)

pop_vtd[pop_vtd %!in% adj_vtd]
adj_vtd[adj_vtd %!in% pop_vtd]


# create data to temporarily merging enclave VTDs with surrounding (neighbor) VTDs
# for use in the splitIntoTwo function

vtd_enclave_xwalk <- adj %>%
  dplyr::group_by(SOURCE_VTDID) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == 1) %>%
  # add VTDs 39081081AAF, 39081081AAE (connected enclaves)
  tibble::add_row(
    SOURCE_VTDID = "39081081AAF",
    NEIGHBOR_VTDID = "39081081AAB"
  ) %>%
  tibble::add_row(
    SOURCE_VTDID = "39081081AAE",
    NEIGHBOR_VTDID = "39081081AAB"
  )  %>%
  # add VTDs 39013013ACX, 39013013ACZ (connected enclaves)
  tibble::add_row(
    SOURCE_VTDID = "39013013ACX",
    NEIGHBOR_VTDID = "39013013ADB"
  ) %>%
  tibble::add_row(
    SOURCE_VTDID = "39013013ACZ",
    NEIGHBOR_VTDID = "39013013ADB"
  ) %>%
  # add VTDs 39171171ABH, 39171171ABG (connected enclaves)
  tibble::add_row(
    SOURCE_VTDID = "39171171ABH",
    NEIGHBOR_VTDID = "39171171ABF"
  ) %>%
  tibble::add_row(
    SOURCE_VTDID = "39171171ABG",
    NEIGHBOR_VTDID = "39171171ABF"
  ) %>%
  dplyr::select(-n)

vtd_enclave_list <- vtd_enclave_xwalk %>%
  dplyr::pull(SOURCE_VTDID)

vtd_neighbor_list <- adj %>%
  dplyr::filter(
    NEIGHBOR_VTDID %in% vtd_enclave_list,
    SOURCE_VTDID %!in% vtd_enclave_list
    ) %>%
  dplyr::pull(SOURCE_VTDID)

vtd_enclave_data <- pop %>%
  dplyr::filter(GEO_ID %in% vtd_enclave_list)

vtd_neighbor_data <- pop %>%
  dplyr::filter(GEO_ID %in% vtd_neighbor_list)

### functions ----------------------------------------------------------------------

tractdist <- function(tracts, adjdf = adj, popdf = pop){
  
  # test for contiguousness
  if(isContig(popdf$GEO_ID, adjdf) == 0){
    return("Data not contiguous.")
  }
  
  # pull list of all tracts to find distance for
  tract_list <- unique(popdf$GEO_ID)
  
  # list of calculated distances
  list_done <- c(tracts)
  
  # list of uncalculated distances
  list_todo <- c(tract_list)
  
  # remove tract1 from the list
  list_todo <- list_todo[!list_todo %in% tracts]
  
  # set up distance counter
  d <- 1
  
  # set up output df
  output <- popdf %>%
    dplyr::select(-CIT_EST) %>%
    dplyr::mutate(dist = NA)
  
  # set tract1 dist equal to 0
  output$dist[output$GEO_ID %in% tracts] <- 0
  
  x <- 1
  
  # while there are still distances to calculate...
  while(length(list_todo) > 0){
    
    # pull all tracts adjacent to a given tract
    next_circle <- adjdf %>%
      dplyr::filter(
        SOURCE_VTDID %in% list_done,
        NEIGHBOR_VTDID %!in% list_done
      ) %>%
      dplyr::pull(NEIGHBOR_VTDID) %>%
      unique()
    
    output <- output %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dist = dplyr::case_when(
        is.na(dist) & GEO_ID %in% next_circle ~ d,
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
        SOURCE_VTDID %in% list_identified,
        # ...but the neighbor is not identified
        !NEIGHBOR_VTDID %in% list_identified) %>%
      dplyr::pull(NEIGHBOR_VTDID) %>%
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


# df <- pop
# adjdf <- adj

splitIntoTwo <- function(df = pop, adjdf = adj){

  # test if the input df is contiguous
  if(isContig(df$GEO_ID, adjdf) == 0){

    stop(paste("Error: Input Not Contiguous"))

  }
  
  # pull enclaves within the inputted df
  vtd_enclaves_df <- df$GEO_ID[df$GEO_ID %in% vtd_enclave_list]
  
  # pull vtds surrounding enclaves within the inputted df
  vtd_neighbor_df <- df$GEO_ID[df$GEO_ID %in% vtd_neighbor_list]
  
  # create a modified population dataset merging the enclaves into their surrounding districts
  # in order to keep them assigned districts together
  df_mod <- df %>%
    dplyr::left_join(vtd_enclave_xwalk,
                     dplyr::join_by("GEO_ID" == "SOURCE_VTDID")) %>%
    dplyr::mutate(GEO_ID = dplyr::coalesce(NEIGHBOR_VTDID, GEO_ID)) %>%
    dplyr::group_by(GEO_ID) %>%
    dplyr::summarise(CIT_EST = sum(CIT_EST, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # create a modified adjacency matrix
  adjdf_mod <- adjdf %>%
    dplyr::filter(
      SOURCE_VTDID %!in% vtd_enclave_list,
      NEIGHBOR_VTDID %!in% vtd_enclave_list
    )
  
  # test if the subset of data is contiguous
    if(isContig(df_mod$GEO_ID, adjdf_mod) == 0){

    stop(paste("Error: Input Not Contiguous"))

  }

  # split df VTD/population frame into two districts
  # of equal population
  
  # list all unassigned districts in the population
  tract_list <- unique(df_mod$GEO_ID)
  
  # select random tract from population
  tract1 <- sample(tract_list, 1)
  
  # exclude tract1 from the sample
  tract_list <- tract_list[!tract_list %in% tract1]
  
  # select second random tract from the new population
  tract2 <- sample(tract_list,1)
  
  # calculate distances to tract1
  t1_df <- tractdist(tract1, adjdf_mod, df_mod) %>%
    dplyr::rename(dist1 = dist) %>%
    tidyr::replace_na(list(x = NA, y = 1))
  # calculate distances to tract2
  t2_df <- tractdist(tract2, adjdf_mod, df_mod) %>%
    dplyr::rename(dist2 = dist) %>%
    tidyr::replace_na(list(x = NA, y = 1))
  
  # join the two distance columns together
  tdf <- dplyr::full_join(t1_df, t2_df,
                          by = c("GEO_ID")) %>%
    # calculate differences of distances
    ## positive - tract2 is closer
    ## negative - tract1 is closer
    dplyr::mutate(
      distfrom = dist2 - dist1,
      GEO_ID = as.character(GEO_ID)
    )
  
  # merge in tract population estimates
  tdf <- dplyr::left_join(tdf, df_mod, by=c("GEO_ID")) %>%
    dplyr::mutate(CIT_EST = as.numeric(CIT_EST))
  
  # set initial cumulative sum ("cmsm") at 0
  cmsm <- 0
  
  # calculate the sum goal
  sum_goal <- (sum(tdf$CIT_EST) / 2) - median(tdf$CIT_EST)
  
  # calculate furthest towards tract1 for starting point
  j <- min(tdf$distfrom, na.rm = TRUE)
  
  # while the cumulative sum is <= the population goal...
  while(cmsm <= sum_goal){
    
    # filter for the distance being closer to equidistant
    tmp <- tdf %>%
      dplyr::filter(distfrom <= j)
    
    # calculate cumulative sum of radius size j
    cmsm <- sum(tmp$CIT_EST, na.rm = TRUE)
    
    # increase radius
    j <- j + 1
  }
  
  # determine middle distance between tract1 and tract2 and pull
  # tracts here
  split_list <- tdf %>%
    dplyr::filter(distfrom == j-1) %>%
    dplyr::pull(GEO_ID)
  
  # calculate all tracts solidly on half "A"
  half_a <- tdf %>%
    dplyr::filter(distfrom < j-1)
  
  # Half "A" population estimate
  pop_half_a <- sum(half_a$CIT_EST)
  alpha <- unique(half_a$GEO_ID)
  
  # calculate all tracts solidly on half "B"
  half_b <- tdf %>%
    dplyr::filter(distfrom >= j)
  
  # Half "B" population estimates
  pop_half_b <- sum(half_b$CIT_EST)
  beta <- unique(half_b$GEO_ID)
  
  # create a df that has the midpoint tracts listed
  whichside <- data.frame(GEO_ID = split_list) %>%
    dplyr::rowwise() %>%
    # for each tract, calculate how many neighboring tracts it has
    # in the adjacency matrix that are in alpha/beta lists, respectively
    # and now many neighboring tracts it has total
    dplyr::mutate(
      alif = adjdf_mod %>%
        dplyr::filter(
          SOURCE_VTDID == as.character(GEO_ID),
          NEIGHBOR_VTDID %in% alpha) %>%
        unique() %>%
        nrow(),
      ba = adjdf_mod %>%
        dplyr::filter(
          SOURCE_VTDID == as.character(GEO_ID),
          NEIGHBOR_VTDID %in% beta) %>%
        unique() %>%
        nrow(),
      alif_total = adjdf_mod %>%
        dplyr::filter(
          SOURCE_VTDID == as.character(GEO_ID)
          ) %>%
        unique() %>%
        nrow(),
      ba_total = adjdf_mod %>%
        dplyr::filter(
          SOURCE_VTDID == as.character(GEO_ID),
          ) %>%
        unique() %>%
        nrow(),
      # calculate percentage of neighboring tracts in each side
      alif_perc = alif / alif_total,
      alif_perc = ifelse(is.nan(alif_perc), 0, alif_perc),
      ba_perc = ba / ba_total,
      ba_perc = ifelse(is.nan(ba_perc), 0, ba_perc)
    )
  
  # tmp_half_a <- whichside %>%
  #   dplyr::filter(alif_perc > ba_perc) %>%
  #   dplyr::pull(GEO_ID)
  # tmp_half_a <- c(half_a$GEO_ID, tmp_half_a)
  # 
  # tmp_half_b <- whichside %>%
  #   dplyr::filter(alif_perc < ba_perc) %>%
  #   dplyr::pull(GEO_ID)
  # tmp_half_b <- c(half_b$GEO_ID, tmp_half_b)
  #   
  # whichside_0 <- whichside %>%
  #   dplyr::filter(
  #     alif_perc == 0,
  #     ba_perc == 0
  #     ) %>%
  #   # for each tract, calculate how many neighboring tracts it has
  #   # in the adjacency matrix that are in tmp_alpha/tmp_beta lists, respectively
  #   # and now many neighboring tracts it has total
  #   dplyr::mutate(
  #     alif_tmp = adjdf_mod %>%
  #       dplyr::filter(
  #         SOURCE_VTDID == as.character(GEO_ID),
  #         NEIGHBOR_VTDID %in% tmp_half_a) %>%
  #       unique() %>%
  #       nrow(),
  #     ba_tmp = adjdf_mod %>%
  #       dplyr::filter(
  #         SOURCE_VTDID == as.character(GEO_ID),
  #         NEIGHBOR_VTDID %in% tmp_half_b) %>%
  #       unique() %>%
  #       nrow(),
  #     alif_tmp_total = adjdf_mod %>%
  #       dplyr::filter(
  #         SOURCE_VTDID == as.character(GEO_ID)
  #       ) %>%
  #       unique() %>%
  #       nrow(),
  #     ba_tmp_total = adjdf_mod %>%
  #       dplyr::filter(
  #         SOURCE_VTDID == as.character(GEO_ID),
  #       ) %>%
  #       unique() %>%
  #       nrow(),
  #     # calculate percentage of neighboring tracts in each side
  #     alif_tmp_perc = alif_tmp / alif_tmp_total,
  #     alif_tmp_perc = ifelse(is.nan(alif_tmp_perc), 0, alif_tmp_perc),
  #     ba_tmp_perc = ba_tmp / ba_tmp_total,
  #     ba_tmp_perc = ifelse(is.nan(ba_tmp_perc), 0, ba_tmp_perc),
  #     # modify temporary alif and ba percs to ensure they are closest to 0 in the whichside df
  #     alif_tmp_perc <- alif_tmp_perc^2,
  #     ba_tmp_perc <- ba_tmp_perc^2
  #   ) %>%
  #   dplyr::select(GEO_ID, alif_tmp_perc, ba_tmp_perc)
  # 
  # whichside <- whichside %>%
  #   dplyr::left_join(whichside_0, by = "GEO_ID") %>%
  #   dplyr::mutate(
  #     alif_perc = dplyr::coalesce(alif_tmp_perc, alif_perc),
  #     ba_perc = dplyr::coalesce(ba_tmp_perc, ba_perc)
  #   ) %>%
  #   dplyr::select(-alif_tmp_perc, -ba_tmp_perc)

  # while the population is <= the population target...
  while(pop_half_a <= sum_goal){
    
    # pull the "middle" tracts and calculate how many more neighboring
    # tracts are on Side A/Side B
    ## positive - Side B has more neighbors
    ## negative - Side A has more neighbors
    temp <- whichside %>%
      dplyr::mutate(side = ba_perc - alif_perc) %>%
      # sort by number of adjacent neighbors with more neighbors
      # in Side A at the top
      dplyr::arrange(side)
    
    # set second cumulative sum (cmsm2) to 0
    cmsm2 <- 0
    q <- 1
    
    # calculate the difference in population to make up between Side A and
    # the original population goal
    sum_goal2 <- sum_goal - pop_half_a - median(tdf$CIT_EST)
    
    # while cmsm2 is <= sum_goal2...
    while(cmsm2 <= sum_goal2){
      
      # pull the q "middle" tracts with the most neighbors in Side A
      tmp <- temp[c(1:q),]
      
      # pull tracts
      tmp_list <- tmp$GEO_ID
      
      # select these tracts
      tmp_df <- tdf %>%
        dplyr::filter(GEO_ID %in% tmp_list)
      
      # calculate the second cumulative sum to see if that brings
      # Side A to the goal
      cmsm2 <- sum(tmp_df$CIT_EST, na.rm = TRUE)
      
      # increase the counter
      q = q + 1
    }
    
    # merge together Side A data
    half_a <- rbind(half_a,tmp_df)
    
    # list of Side A tracts
    half_a_list <- unique(half_a$GEO_ID)
    
    # calculate Side A population
    pop_half_a <- sum(half_a$CIT_EST, na.rm = TRUE)
  }
  
  # in the original df, code tracts as within Side A
  # or Side B
  tdf <- tdf %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      half = dplyr::case_when(
        GEO_ID %in% half_a_list ~ 1,
        .default = 2
    ))
  
  # pull district coding for VTDs surrounding enclaves
  tdf_surrounding <- tdf %>%
    dplyr::filter(GEO_ID %in% vtd_neighbor_df) %>%
    dplyr::rename(Geography_combined = GEO_ID)
  
  # correct combined VTDs with neighbor-only Populations
  tdf_neighbor_to_add <- tdf_surrounding %>%
    dplyr::left_join(df,
                     dplyr::join_by("Geography_combined" == "GEO_ID")) %>%
    dplyr::select(-CIT_EST.x) %>%
    dplyr::rename(
      CIT_EST = CIT_EST.y,
      GEO_ID = Geography_combined)
  
  # pull data for enclave VTDs and assign districts of their neighbor VTDs
  tdf_enclave_to_add <- df %>%
    dplyr::filter(GEO_ID %in% vtd_enclaves_df) %>%
    dplyr::left_join(vtd_enclave_xwalk,
                     dplyr::join_by("GEO_ID" == "SOURCE_VTDID")) %>%
    dplyr::left_join(tdf_neighbor_to_add %>%
                       dplyr::select(-CIT_EST),
                     dplyr::join_by("NEIGHBOR_VTDID" == "GEO_ID")) %>%
    dplyr::select(-NEIGHBOR_VTDID)
  
  tdf <- tdf %>%
    # remove combined VTD data from the dataset
    dplyr::filter(GEO_ID %!in% vtd_neighbor_df) %>%
    # bind neighbor and enclave data
    rbind(tdf_neighbor_to_add,
          tdf_enclave_to_add) %>%
    as.data.frame()
  
  half1_list_tmp <- tdf %>%
    dplyr::filter(half == 1) %>%
    dplyr::pull(GEO_ID)
  
  half2_list_tmp <- tdf %>%
    dplyr::filter(half == 2) %>%
    dplyr::pull(GEO_ID)
  
  # pull number of adjacent tracts and tracts in each half to determine non-contiguousness
  tdf <- tdf %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_adj = adjdf %>%
        dplyr::filter(SOURCE_VTDID == GEO_ID) %>%
        nrow(),
      half1_adj = adjdf %>%
        dplyr::filter(
          SOURCE_VTDID == GEO_ID,
          NEIGHBOR_VTDID %in% half1_list_tmp
          ) %>%
        nrow(),
      half2_adj = adjdf %>%
        dplyr::filter(
          SOURCE_VTDID == GEO_ID,
          NEIGHBOR_VTDID %in% half2_list_tmp
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
    dplyr::pull(GEO_ID) %>%
    unique()
  
  # pull data for Side B
  half2_list <- tdf %>%
    dplyr::filter(half == 2) %>%
    dplyr::pull(GEO_ID) %>%
    unique()
  
  # create new population sets for Side A...
  pop_half1 <- df %>%
    dplyr::filter(GEO_ID %in% half1_list)
  # ...and Side B
  pop_half2 <- df %>%
    dplyr::filter(GEO_ID %in% half2_list)
  
  # create new adjacency matrices for Side A...
  adj_half1 <- adjdf %>%
    dplyr::filter(SOURCE_VTDID %in% half1_list,
                  NEIGHBOR_VTDID %in% half1_list
    )
  # ...and Side B
  adj_half2 <- adjdf %>%
    dplyr::filter(SOURCE_VTDID %in% half2_list,
                  NEIGHBOR_VTDID %in% half2_list
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

split1 <- splitIntoTwo(pop, adj)

pop_half1 <- split1[[1]]
pop_half2 <- split1[[2]]

adj_half1 <- split1[[3]]
adj_half2 <- split1[[4]]

# population totals for each half
sum(pop_half1$CIT_EST, na.rm = TRUE)
sum(pop_half2$CIT_EST, na.rm = TRUE)


### Split 2: Four Parts ----------------------------------------------------------------------

#### Split 2A --------------------------------------------------------------------------------
# Split Half1 into Quarter1 and Quarter2

split2a <- splitIntoTwo(pop_half1, adj_half1)
pop_quarter1 <- split2a[[1]]
pop_quarter2 <- split2a[[2]]

adj_quarter1 <- split2a[[3]]
adj_quarter2 <- split2a[[4]]

#### Split 2B --------------------------------------------------------------------------------
# Split Half2 into Quarter3 and Quarter4

split2b <- splitIntoTwo(pop_half2, adj_half2)
pop_quarter3 <- split2b[[1]]
pop_quarter4 <- split2b[[2]]

adj_quarter3 <- split2b[[3]]
adj_quarter4 <- split2b[[4]]

#### Split 2 Populations --------------------------------------------------------------------------------

# population totals for each quarter
sum(pop_quarter1$CIT_EST, na.rm = TRUE)
sum(pop_quarter2$CIT_EST, na.rm = TRUE)
sum(pop_quarter3$CIT_EST, na.rm = TRUE)
sum(pop_quarter4$CIT_EST, na.rm = TRUE)


### Split 3: Eight Parts ----------------------------------------------------------------------

#### Split 3A --------------------------------------------------------------------------------
# Split Quarter1 into Eigth1 and Eigth2

split3a <- splitIntoTwo(pop_quarter1, adj_quarter1)
pop_eigth1 <- split3a[[1]]
pop_eigth2 <- split3a[[2]]

adj_eigth1 <- split3a[[3]]
adj_eigth2 <- split3a[[4]]

#### Split 3B --------------------------------------------------------------------------------
# Split Quarter2 into Eigth3 and Eigth4

split3b <- splitIntoTwo(pop_quarter2, adj_quarter2)
pop_eigth3 <- split3b[[1]]
pop_eigth4 <- split3b[[2]]

adj_eigth3 <- split3b[[3]]
adj_eigth4 <- split3b[[4]]

#### Split 3C --------------------------------------------------------------------------------
# Split Quarter3 into Eigth5 and Eigth6

split3c <- splitIntoTwo(pop_quarter3, adj_quarter3)
pop_eigth5 <- split3c[[1]]
pop_eigth6 <- split3c[[2]]

adj_eigth5 <- split3c[[3]]
adj_eigth6 <- split3c[[4]]

#### Split 3D --------------------------------------------------------------------------------
# Split Quarter4 into Eigth7 and Eigth8

split3d <- splitIntoTwo(pop_quarter4, adj_quarter4)
pop_eigth7 <- split3d[[1]]
pop_eigth8 <- split3d[[2]]

adj_eigth7 <- split3d[[3]]
adj_eigth8 <- split3d[[4]]

#### Split 3 Populations --------------------------------------------------------------------------------

# population totals for each quarter
sum(pop_eigth1$CIT_EST, na.rm = TRUE)
sum(pop_eigth2$CIT_EST, na.rm = TRUE)
sum(pop_eigth3$CIT_EST, na.rm = TRUE)
sum(pop_eigth4$CIT_EST, na.rm = TRUE)
sum(pop_eigth5$CIT_EST, na.rm = TRUE)
sum(pop_eigth6$CIT_EST, na.rm = TRUE)
sum(pop_eigth7$CIT_EST, na.rm = TRUE)
sum(pop_eigth8$CIT_EST, na.rm = TRUE)


### Split 4: Sixteen Parts ----------------------------------------------------------------------

#### Split 4A --------------------------------------------------------------------------------
# Split Eigth1 into Sixteenth1 and Sixteenth2

split4a <- splitIntoTwo(pop_eigth1, adj_eigth1)
pop_sixteenth1 <- split4a[[1]]
pop_sixteenth2 <- split4a[[2]]

adj_sixteenth1 <- split4a[[3]]
adj_sixteenth2 <- split4a[[4]]

#### Split 4B --------------------------------------------------------------------------------
# Split Eigth2 into Sixteenth3 and Sixteenth4

split4b <- splitIntoTwo(pop_eigth2, adj_eigth2)
pop_sixteenth3 <- split4b[[1]]
pop_sixteenth4 <- split4b[[2]]

adj_sixteenth3 <- split4b[[3]]
adj_sixteenth4 <- split4b[[4]]

#### Split 4C --------------------------------------------------------------------------------
# Split Eigth3 into Sixteenth5 and Sixteenth6

split4c <- splitIntoTwo(pop_eigth3, adj_eigth3)
pop_sixteenth5 <- split4c[[1]]
pop_sixteenth6 <- split4c[[2]]

adj_sixteenth5 <- split4c[[3]]
adj_sixteenth6 <- split4c[[4]]

#### Split 4D --------------------------------------------------------------------------------
# Split Eigth4 into Sixteenth7 and Sixteenth8

split4d <- splitIntoTwo(pop_eigth4, adj_eigth4)
pop_sixteenth7 <- split4d[[1]]
pop_sixteenth8 <- split4d[[2]]

adj_sixteenth7 <- split4d[[3]]
adj_sixteenth8 <- split4d[[4]]

#### Split 4E --------------------------------------------------------------------------------
# Split Eigth5 into Sixteenth9 and Sixteenth10

split4e <- splitIntoTwo(pop_eigth5, adj_eigth5)
pop_sixteenth9 <- split4e[[1]]
pop_sixteenth10 <- split4e[[2]]

adj_sixteenth9 <- split4e[[3]]
adj_sixteenth10 <- split4e[[4]]

#### Split 4F --------------------------------------------------------------------------------
# Split Eigth6 into Sixteenth11 and Sixteenth12

split4f <- splitIntoTwo(pop_eigth6, adj_eigth6)
pop_sixteenth11 <- split4f[[1]]
pop_sixteenth12 <- split4f[[2]]

adj_sixteenth11 <- split4f[[3]]
adj_sixteenth12 <- split4f[[4]]

#### Split 4G --------------------------------------------------------------------------------
# Split Eigth7 into Sixteenth13 and Sixteenth14

split4g <- splitIntoTwo(pop_eigth7, adj_eigth7)
pop_sixteenth13 <- split4g[[1]]
pop_sixteenth14 <- split4g[[2]]

adj_sixteenth13 <- split4g[[3]]
adj_sixteenth14 <- split4g[[4]]

#### Split 4H --------------------------------------------------------------------------------
# Split Eigth8 into Sixteenth15 and Sixteenth16

split4h <- splitIntoTwo(pop_eigth8, adj_eigth8)
pop_sixteenth15 <- split4h[[1]]
pop_sixteenth16 <- split4h[[2]]

adj_sixteenth15 <- split4h[[3]]
adj_sixteenth16 <- split4h[[4]]

#### Split 4 Populations --------------------------------------------------------------------------------

# population totals for each quarter
sum(pop_sixteenth1$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth2$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth3$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth4$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth5$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth6$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth7$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth8$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth9$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth10$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth11$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth12$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth13$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth14$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth15$CIT_EST, na.rm = TRUE)
sum(pop_sixteenth16$CIT_EST, na.rm = TRUE)


### Create Final Dataset ----------------------------------------------------------------------

pop_final <- pop %>%
  dplyr::mutate(
    district = dplyr::case_when(
      GEO_ID %in% pop_sixteenth1$GEO_ID ~ 1,
      GEO_ID %in% pop_sixteenth2$GEO_ID ~ 2,
      GEO_ID %in% pop_sixteenth3$GEO_ID ~ 3,
      GEO_ID %in% pop_sixteenth4$GEO_ID ~ 4,
      GEO_ID %in% pop_sixteenth5$GEO_ID ~ 5,
      GEO_ID %in% pop_sixteenth6$GEO_ID ~ 6,
      GEO_ID %in% pop_sixteenth7$GEO_ID ~ 7,
      GEO_ID %in% pop_sixteenth8$GEO_ID ~ 8,
      GEO_ID %in% pop_sixteenth9$GEO_ID ~ 9,
      GEO_ID %in% pop_sixteenth10$GEO_ID ~ 10,
      GEO_ID %in% pop_sixteenth11$GEO_ID ~ 11,
      GEO_ID %in% pop_sixteenth12$GEO_ID ~ 12,
      GEO_ID %in% pop_sixteenth13$GEO_ID ~ 13,
      GEO_ID %in% pop_sixteenth14$GEO_ID ~ 14,
      GEO_ID %in% pop_sixteenth15$GEO_ID ~ 15,
      GEO_ID %in% pop_sixteenth16$GEO_ID ~ 16,
      .default = NA
    )
  )

table(pop_final$district, useNA = "always")

write.csv(pop_final, "District Outputs Rodden/output_vtd04.csv", row.names = FALSE)


### Create Map -----------------------------------------------------------------------

color_palette <- c("dodgerblue3","firebrick2","chartreuse2","darkorchid3","darkslategray4",
                   "orange2","lightsalmon","hotpink1","turquoise","darkseagreen2","lightblue3",
                   "mediumorchid1","plum","ivory2","goldenrod","olivedrab3")

color_palette <- c("#101034","#104533","#014589","#169308","#293940",
                   "#101034","#104533","#014589","#169308","#293940",
                   "#101034","#104533","#014589","#169308","#293940",
                   "#fff449")

map <- pop_final %>%
  # merge tract prefix to GEO_ID prior to merging
  dplyr::left_join(area,
                   dplyr::join_by("GEO_ID" == "geoid10"))

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
  scale_fill_manual(values = color_palette) +
  theme_void()

map



map_mapview <- pop_final %>%
  dplyr::left_join(area,
                   dplyr::join_by("GEO_ID" == "geoid10")) %>%
  dplyr::mutate(color = dplyr::case_when(
    district == 1 ~ "dodgerblue3",
    district == 2 ~ "firebrick2",
    district == 3 ~ "chartreuse2",
    district == 4 ~ "darkorchid3",
    district == 5 ~ "darkslategray4",
    district == 6 ~ "orange2",
    district == 7 ~ "lightsalmon",
    district == 8 ~ "hotpink1",
    district == 9 ~ "turquoise",
    district == 10 ~ "darkseagreen2",
    district == 11 ~ "lightblue3",
    district == 12 ~ "mediumorchid1",
    district == 13 ~ "plum",
    district == 14 ~ "ivory2",
    district == 15 ~ "goldenrod",
    district == 16 ~ "olivedrab3",
    .default = "white"
  )) %>%
  st_as_sf()

mapview(map_mapview, zcol = "color")

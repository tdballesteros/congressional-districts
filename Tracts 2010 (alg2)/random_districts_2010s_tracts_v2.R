
# This file computes Ohio's 16 congressional districts using the 2010 US Census data and 2020
# election results, which used districts drawn from the 2010 Census data.

# v2 changes:
## Adjacency matrix is calculated from the US Census Bureau / TIGER/Line shapefile directly and
## no longer uses queen's adjacency (diagonals)



### load libraries ----------------------------------------------------------------------
library(readxl)
library(sf)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------
## Adjacency Matrix
# Source: Diversity and Disparities Project, Brown University
# Source Website: https://s4.ad.brown.edu/projects/diversity/Researcher/Pooling.htm
# NOTE: 4 tracts in the adjacency matrix are not present in the tracts shapefile;
# they are still included in all data calculations, but are not mapped.
# Tracts: 39007990000, 39035990000, 39093990200, 39095990000
adj <- read.csv("Data/adjacency_list_tracts_2010.csv",
                colClasses = "character")

## Population Data
# Source: US Census Bureau
# tract data
tract_data <- read.csv("Data/population_data_2010_by_tract.csv",
                       skip = 1,
                       colClasses = "character")

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10)


### format data ----------------------------------------------------------------------
pop_tracts_total <- tract_data %>%
  dplyr::filter(
    # Ohio FIPS code is 39
    stringr::str_sub(Geography, 10, 11) == "39",
    # filter out total row for Ohio
    Geography != "0400000US39"
    ) %>%
  dplyr::select(Geography, Total) %>%
  dplyr::rename(Population = Total) %>%
  # several tracts have non-numeric entries in the Population  field; manually correct these
  # and convert all data to numeric values
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


# # modify by merging enclave VTDs with surrounding VTD and combining populations,
# # as these VTDs always have to stay paired with each other
# 
# # source is the enclave, neighbor is the surrounding vtd
# vtd_enclave_data <- adj_vtd %>%
#   dplyr::group_by(SOURCE_TRACTID) %>%
#   dplyr::mutate(n = n()) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(n == 1) %>%
#   dplyr::select(-n) %>%
#   dplyr::rename(
#     vtd_enclave = SOURCE_TRACTID,
#     vtd_surrounding = NEIGHBOR_TRACTID
#   ) %>%
#   # add VTDs 39081081AAF, 39081081AAE (connected enclaves)
#   tibble::add_row(
#     vtd_enclave = "39081081AAF",
#     vtd_surrounding = "39081081AAB"
#   ) %>%
#   tibble::add_row(
#     vtd_enclave = "39081081AAE",
#     vtd_surrounding = "39081081AAB"
#   )
# 
# vtd_enclave_list <- vtd_enclave_data %>%
#   dplyr::pull(vtd_enclave)
# 
# pop_vtds_total_mod <- dplyr::full_join(pop_vtds_total,
#                                        vtd_enclave_data,
#                                        dplyr::join_by("Geography" == "vtd_enclave")) %>%
#   dplyr::mutate(Geography = dplyr::coalesce(vtd_surrounding, Geography)) %>%
#   dplyr::relocate(Geography, .before = Population) %>%
#   dplyr::select(-vtd_surrounding) %>%
#   dplyr::group_by(Geography) %>%
#   dplyr::summarise(Population = sum(Population, na.rm = TRUE)) %>%
#   dplyr::ungroup()


adj <- adj %>%
  # filter out all non-Ohio tracts
  dplyr::filter(
    stringr::str_sub(SOURCE_TRACTID, 1, 2) == "39",
    stringr::str_sub(NEIGHBOR_TRACTID, 1, 2) == "39"
  )

# create modified vtd adjacency matrix without enclaves
# adj_vtd_mod <- adj_vtd %>%
#   dplyr::filter(
#     SOURCE_TRACTID %!in% c(vtd_enclave_list, "3906935AAH"),
#     NEIGHBOR_TRACTID %!in% c(vtd_enclave_list, "3906935AAH")
#     )

### functions ----------------------------------------------------------------------

tractdist <- function(tracts, adjdf = adj, popdf = pop_tracts_total){
  
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
  
  # remove tract1 from the list
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


splitIntoTwo <- function(df = pop_tracts_total, adjdf = adj){
  
  # test if the input df is contiguous
  if(isContig(df$Geography, adjdf) == 0){
    
    stop(paste("Error: Input Not Contiguous"))
    
  }
  
  # split df tract/population frame into two districts
  # of equal population
  
  # list all unassigned districts in the population
  tract_list <- unique(df$Geography)
  
  # select random tract from population
  tract1 <- sample(tract_list, 1)
  
  # exclude tract1 from the sample
  tract_list <- tract_list[!tract_list %in% tract1]
  
  # select second random tract from the new population
  tract2 <- sample(tract_list,1)
  
  # calculate distances to tract1
  t1_df <- tractdist(tract1, adjdf, df) %>%
    dplyr::rename(dist1 = dist) %>%
    tidyr::replace_na(list(x = NA, y = 1))
  # calculate distances to tract2
  t2_df <- tractdist(tract2, adjdf, df) %>%
    dplyr::rename(dist2 = dist) %>%
    tidyr::replace_na(list(x = NA, y = 1))
  
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
  tdf <- dplyr::left_join(tdf, df, by=c("Geography")) %>%
    dplyr::mutate(Population = as.numeric(Population))
  
  # set initial cumulative sum ("cmsm") at 0
  cmsm <- 0
  
  # calculate the sum goal
  sum_goal <- (sum(tdf$Population) / 2) - median(tdf$Population)
  
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
    sum_goal2 <- sum_goal - pop_half_a - median(tdf$Population)
    
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
    half_a <- rbind(half_a,tmp_df)
    
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

split1 <- splitIntoTwo(pop_tracts_total, adj)

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
sum(pop_quarter1$Population, na.rm = TRUE)
sum(pop_quarter2$Population, na.rm = TRUE)
sum(pop_quarter3$Population, na.rm = TRUE)
sum(pop_quarter4$Population, na.rm = TRUE)


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

write.csv(pop_final, "District Outputs Tracts 2010/output30.csv", row.names = FALSE)


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
  # merge tract prefix to GEO_ID prior to merging
  dplyr::mutate(Geography = paste0("1400000US",Geography)) %>%
  dplyr::left_join(shape_tract,
                   by = "Geography") %>%
  st_as_sf()

mapview(map_mapview, zcol = "district", col.regions = mapviewPalette("mapviewSpectralColors"))



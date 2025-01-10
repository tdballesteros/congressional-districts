
### load libraries ----------------------------------------------------------------------

library(readxl)
library(sf)
library(igraph)
library(RColorBrewer)
library(tidyverse)


### 'not in' function ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

# adjacency matrix
adj <- read.csv("Data/adj2.csv",
                colClasses = "character")

# tract list with populations
tract_data <- read.csv("Data/BlockGr_Ohio.csv")

# Ohio shapefile
area <- sf::read_sf(
  dsn = "Data/gz_2010_39_140_00_500k/gz_2010_39_140_00_500k.shp") %>%
  dplyr::rename_with(tolower, -starts_with("GEO_ID"))


### format data ----------------------------------------------------------------------

pop2 <- tract_data %>%
  dplyr::mutate(geoid = as.character(geoid)) %>%
  dplyr::filter(lntitle == "Total") %>%
  
  # extract FIPS data
  dplyr::mutate(
    stateid = str_sub(geoid, 8, 9),
    countyid = str_sub(geoid, 10, 12),
    tract = str_sub(geoid, 13, 18),
    block = str_sub(geoid, 19, 19)
  ) %>%
  
  # Ohio FIPS code is 39
  dplyr::filter(stateid == 39)

pop <- pop2 %>%
  dplyr::mutate(geoid = str_sub(geoid, -12, -2)) %>%
  dplyr::group_by(geoid) %>%
  dplyr::summarise(CIT_EST = sum(CIT_EST, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(GEO_ID = geoid)

pop2 <- pop2 %>%
  dplyr::select(geoid, block) %>%
  dplyr::mutate(
    GEO_ID = str_sub(pop2$geoid, 8, 18),
    block2 = paste0(GEO_ID,block)
  ) %>%
  dplyr::select(GEO_ID, block2)

# other setup

tract_list <- unique(adj$SOURCE_TRACTID)
rand <- sample(tract_list, 1)
tract_df <- data.frame(
  tract = tract_list,
  dist = NA
)


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


splitIntoTwo <- function(df = pop, adjdf = adj){
  
  # test if the input df is contiguous
  if(isContig2(pop$GEO_ID, adj) == 0){
    
    return(paste("Error: Input Not Contiguous"))
    
  }
  
  # split "pop" tract/population frame into two districts
  # of equal population
  
  # list all unassigned districts in the population
  tract_list <- unique(df$GEO_ID)
  
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
                          by = c("GEO_ID")) %>%
    # calculate differences of distances
    ## positive - tract2 is closer
    ## negative - tract1 is closer
    dplyr::mutate(
      distfrom = dist2 - dist1,
      GEO_ID = as.character(GEO_ID)
    )
  
  # merge in tract population estimates
  tdf <- dplyr::left_join(tdf, pop, by=c("GEO_ID")) %>%
    dplyr::mutate(CIT_EST = as.numeric(CIT_EST))
  
  # set initial cumulative sum ("cmsm") at 0
  cmsm <- 0
  
  # calculate the sum goal
  sum_goal <- (sum(tdf$CIT_EST) / 2) - median(tdf$CIT_EST)
  
  # calculate how close to equidistant the closest to equidistant tracts are
  # a value of 0 is equidistant
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
    dplyr::mutate(
      alif = adjdf %>%
        dplyr::filter(
          SOURCE_TRACTID == as.character(GEO_ID),
          NEIGHBOR_TRACTID %in% alpha) %>%
        unique() %>%
        nrow(),
      ba = adjdf %>%
        dplyr::filter(
          SOURCE_TRACTID == as.character(GEO_ID),
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
    dplyr::mutate(half = dplyr::case_when(
      GEO_ID %in% half_a_list ~ 1,
      .default = 2
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
  pop_half1 <- pop %>%
    dplyr::filter(GEO_ID %in% half1_list)
  # ...and Side B
  pop_half2 <- pop %>%
    dplyr::filter(GEO_ID %in% half2_list)
  
  # create new adjacency matrices for Side A...
  adj_half1 <- adj %>%
    dplyr::filter(SOURCE_TRACTID %in% half1_list,
                  NEIGHBOR_TRACTID %in% half1_list
    )
  # ...and Side B
  adj_half2 <- adj %>%
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
    return(paste("Error: Function produced a non-contiguous district."))
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

#### Split 3A --------------------------------------------------------------------------------
# Split Eigth1 into Sixteenth1 and Sixteenth2

split4a <- splitIntoTwo(pop_eigth1, adj_eigth1)
pop_sixteenth1 <- split4a[[1]]
pop_sixteenth2 <- split4a[[2]]

adj_sixteenth1 <- split4a[[3]]
adj_sixteenth2 <- split4a[[4]]

#### Split 3B --------------------------------------------------------------------------------
# Split Eigth2 into Sixteenth3 and Sixteenth4

split4b <- splitIntoTwo(pop_eigth2, adj_eigth2)
pop_sixteenth3 <- split4b[[1]]
pop_sixteenth4 <- split4b[[2]]

adj_sixteenth3 <- split4b[[3]]
adj_sixteenth4 <- split4b[[4]]

#### Split 3C --------------------------------------------------------------------------------
# Split Eigth3 into Sixteenth5 and Sixteenth6

split4c <- splitIntoTwo(pop_eigth3, adj_eigth3)
pop_sixteenth5 <- split4c[[1]]
pop_sixteenth6 <- split4c[[2]]

adj_sixteenth5 <- split4c[[3]]
adj_sixteenth6 <- split4c[[4]]

#### Split 3D --------------------------------------------------------------------------------
# Split Eigth4 into Sixteenth7 and Sixteenth8

split4d <- splitIntoTwo(pop_eigth4, adj_eigth4)
pop_sixteenth7 <- split4d[[1]]
pop_sixteenth8 <- split4d[[2]]

adj_sixteenth7 <- split4d[[3]]
adj_sixteenth8 <- split4d[[4]]

#### Split 3E --------------------------------------------------------------------------------
# Split Eigth5 into Sixteenth9 and Sixteenth10

split4e <- splitIntoTwo(pop_eigth5, adj_eigth5)
pop_sixteenth9 <- split4e[[1]]
pop_sixteenth10 <- split4e[[2]]

adj_sixteenth9 <- split4e[[3]]
adj_sixteenth10 <- split4e[[4]]

#### Split 3F --------------------------------------------------------------------------------
# Split Eigth6 into Sixteenth11 and Sixteenth12

split4f <- splitIntoTwo(pop_eigth6, adj_eigth6)
pop_sixteenth11 <- split4f[[1]]
pop_sixteenth12 <- split4f[[2]]

adj_sixteenth11 <- split4f[[3]]
adj_sixteenth12 <- split4f[[4]]

#### Split 3G --------------------------------------------------------------------------------
# Split Eigth7 into Sixteenth13 and Sixteenth14

split4g <- splitIntoTwo(pop_eigth7, adj_eigth7)
pop_sixteenth13 <- split4g[[1]]
pop_sixteenth14 <- split4g[[2]]

adj_sixteenth13 <- split4g[[3]]
adj_sixteenth14 <- split4g[[4]]

#### Split 3H --------------------------------------------------------------------------------
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

# table(pop_final$district, useNA = "always")


### Create Map -----------------------------------------------------------------------

map <- pop_final %>%
  # merge tract prefix to GEO_ID prior to merging
  dplyr::mutate(GEO_ID = paste0("1400000US",GEO_ID)) %>%
  dplyr::left_join(area,
                   by = "GEO_ID") %>%
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
  st_as_sf %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = color))


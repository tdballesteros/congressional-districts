

### load libraries ----------------------------------------------------------------------

library(readxl)
library(sf)
library(tibble)
library(tidyverse)


### 'not in' function ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

# adj2acency matrix
adj <- read.csv("Data/adjacency_list_tracts_2000.csv",
                colClasses = "character")

# tract list with populations
tract_data <- read.csv("Data/population_data_2000_by_tract.csv")

# Ohio shapefile
area <- sf::read_sf(
  dsn = "Data/gz_2010_39_140_00_500k/gz_2010_39_140_00_500k.shp") %>%
  dplyr::rename_with(tolower, -starts_with("GEO_ID"))


### format data ----------------------------------------------------------------------

pop <- tract_data %>%
  dplyr::mutate(geoid = as.character(geoid)) %>%
  dplyr::filter(lntitle == "Total") %>%
  
  # extract FIPS data
  dplyr::mutate(
    stateid = str_sub(geoid, 8, 9),
    countyid = str_sub(geoid, 10, 12),
    tract = str_sub(geoid, 13, 18),
    block = str_sub(geoid, 19, 19),
    GEO_ID = str_sub(geoid, -12, -2)
  ) %>%
  
  # Ohio FIPS code is 39
  dplyr::filter(stateid == 39) %>%
  dplyr::group_by(GEO_ID) %>%
  dplyr::summarise(CIT_EST = sum(CIT_EST, na.rm = TRUE)) %>%
  dplyr::ungroup()

adj2 <- adj %>%
  dplyr::mutate(adjacent = 1) %>%
  tidyr::pivot_wider(names_from = "NEIGHBOR_TRACTID", values_from = "adjacent") %>%
  # change first column to row names and remove column
  tibble::column_to_rownames(var = "SOURCE_TRACTID")
  
# replace all NA values with 0
adj2[is.na(adj2)] <- 0

# arrange columns in numeric order
adj2 <- adj2[, order(names(adj2))]
# arrange rows in numeric order
adj2 <- adj2[order(attr(adj2,"row.names")),]

# recalculate adjacency matrix elements
for(j in 1:ncol(adj2)){
  for(i in 1:nrow(adj2)){
    row_name <- attr(adj2,"row.names")[i]
    col_name <- names(adj2)[j]
    if(adj2[i,j] == 1){
      adj2[i,j] <- -1/nrow(adj %>% dplyr::filter(SOURCE_TRACTID == row_name))
    } else if(row_name == col_name){
      adj2[i,j] <- 1
    }
  }}

# compute eigenvalues based on the modified adjacency matrix
eigs <- eigen(adj2)

tract_match <- row.names(adj2) %>%
  as.data.frame()
eigval <- cbind(tract_match,eigs$values)
names(eigval) <- c("GEO_ID","eig")

# join eigenvalues with population data
eigval2 <- dplyr::left_join(eigval,pop,by=c("GEO_ID")) %>%
  dplyr::arrange(eig) %>%
  # create cumulative population sum column
  dplyr::mutate(cmsm = cumsum(CIT_EST))

# calculate target population (half of population)
targpop <- sum(eigval2$CIT_EST, na.rm = TRUE) / 2

# identify maximum tract that has a cmsm population below the target
below_cmsm_max <- max(which(eigval2$cmsm < targpop))

halfalpha <- eigval2[c(1:(below_cmsm_max+1)),1] %>%
  na.omit() %>%
  unique()

# add tract prefix
alpha_list <- paste0("1400000US",halfalpha)


### map data ----------------------------------------------------------------------

area %>%
  dplyr::mutate(district = dplyr::case_when(
    GEO_ID %in% alpha_list ~ 1,
    .default = 0
  )) %>%
  dplyr::mutate(color = dplyr::case_when(
    district == 1 ~ "dodgerblue3",
    .default = "firebrick2",
  )) %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = color))


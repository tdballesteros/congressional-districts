
# Create adjacency and distance matrices for Ohio's 2010 Census tracts, not using queen's adjacency (a single 
# shared boundary point does not meet the contiguity condition and more than one shared point is required;
# note that more than one shared boundary point does not necessarily mean a shared boundary line).

# Create second versions without select tracts: 39095990000, 39043990100, 39093990200, 39035990000, 39085990000, 39007990000
# These tracts solely contain water and do not connect any island tracts to the mainland.


### load libraries ----------------------------------------------------------------------
library(sf)
library(igraph)
library(spdep)
library(mapview)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
ohio2010shapefile <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10)

## Population Data
# Source: US Census Bureau
population_data <- read.csv("Data/population_data_2010_by_tract.csv",
                            skip = 1,
                            colClasses = "character")

## Voting Data
# Source: Redistricting Data Hub (RDH)
# Source Website: https://redistrictingdatahub.org/state/ohio/
voting_data <- read.csv("Data/ohio_2020_election_data_by_block.csv",
                        colClasses = "character")


### format data ----------------------------------------------------------------------

tracts_to_remove <- c("39095990000", "39043990100", "39093990200", "39035990000", "39085990000", "39007990000")
tracts_to_remove_df <- data.frame(
  COUNTYFP20 = c("095", "043", "093", "035", "085", "007"),
  TRACTCE20 = c("990000", "990100", "990200", "990000", "990000", "990000"),
  remove_flag = rep(1, 6))

# verify no population data for the above districts
tracts_to_remove_population <- population_data %>%
  dplyr::filter(Geography %in% paste0("1400000US", tracts_to_remove)) %>%
  dplyr::mutate(across(c("Total"), as.numeric)) %>%
  dplyr::summarise(across(c("Total"), sum))
# verified population value is 0

# verify no voting data for the above districts
tracts_to_remove_voting <- voting_data %>%
  dplyr::left_join(tracts_to_remove_df, by = c("COUNTYFP20", "TRACTCE20")) %>%
  dplyr::filter(remove_flag == 1) %>%
  dplyr::mutate(across(c("VAP_MOD", "G20PREDBID", "G20PREGHAW", "G20PRELJOR", "G20PRERTRU"), as.numeric)) %>%
  dplyr::summarise(across(c("VAP_MOD", "G20PREDBID", "G20PREGHAW", "G20PRELJOR", "G20PRERTRU"), sum)) %>%
  dplyr::ungroup()
# verified all values are 0

ohio2010shapefile_v2 <- ohio2010shapefile %>%
  dplyr::filter(Geography %!in% tracts_to_remove)


### create adjacency matrices ----------------------------------------------------------------------

adjacency_matrix <- ohio2010shapefile %>%
  # convert Geography variable as row names
  tibble::column_to_rownames(var = "Geography") %>%
  # convert to an sf object
  sf::st_as_sf() %>%
  # construct neighbor list
  spdep::poly2nb(queen = FALSE) %>%
  # convert neighbor list to adjacency matrix
  spdep::nb2mat(style = "B") %>%
  as.data.frame()

# arrange columns in numeric order
adjacency_matrix <- adjacency_matrix[, order(names(adjacency_matrix))]
# arrange rows in numeric order
adjacency_matrix <- adjacency_matrix[order(attr(adjacency_matrix,"row.names")),]

names(adjacency_matrix) <- row.names(adjacency_matrix)
adjacency_matrix <- adjacency_matrix %>%
  # convert row names to a variable column
  tibble::rownames_to_column(var = "SOURCE_TRACTID")

# create adjacency list
adjacency_list <- adjacency_matrix %>%
  tidyr::pivot_longer(2:2953, names_to = "NEIGHBOR_TRACTID", values_to = "Value") %>%
  dplyr::filter(Value == 1) %>%
  dplyr::select(-Value)
  

adjacency_matrix_v2 <- ohio2010shapefile_v2 %>%
  # convert Geography variable as row names
  tibble::column_to_rownames(var = "Geography") %>%
  # convert to an sf object
  sf::st_as_sf() %>%
  # construct neighbor list
  spdep::poly2nb(queen = FALSE) %>%
  # convert neighbor list to adjacency matrix
  spdep::nb2mat(style = "B") %>%
  as.data.frame()

# arrange columns in numeric order
adjacency_matrix_v2 <- adjacency_matrix_v2[, order(names(adjacency_matrix_v2))]
# arrange rows in numeric order
adjacency_matrix_v2 <- adjacency_matrix_v2[order(attr(adjacency_matrix_v2,"row.names")),]

names(adjacency_matrix_v2) <- row.names(adjacency_matrix_v2)
adjacency_matrix_v2 <- adjacency_matrix_v2 %>%
  # convert row names to a variable column
  tibble::rownames_to_column(var = "SOURCE_TRACTID")

# create adjacency list
adjacency_list_v2 <- adjacency_matrix_v2 %>%
  tidyr::pivot_longer(2:2946, names_to = "NEIGHBOR_TRACTID", values_to = "Value") %>%
  dplyr::filter(Value == 1) %>%
  dplyr::select(-Value)


### create distance matrices ----------------------------------------------------------------------

distance_matrix <- adjacency_matrix %>%
  # reconvert SOURCE_TRACTID to row names
  tibble::column_to_rownames(var = "SOURCE_TRACTID") %>%
  as.matrix() %>%
  igraph::graph_from_adjacency_matrix(mode = "undirected", diag = FALSE) %>%
  igraph::distances()

distance_matrix_v2 <- adjacency_matrix_v2 %>%
  # reconvert SOURCE_TRACTID to row names
  tibble::column_to_rownames(var = "SOURCE_TRACTID") %>%
  as.matrix() %>%
  igraph::graph_from_adjacency_matrix(mode = "undirected", diag = FALSE) %>%
  igraph::distances()


### export files ----------------------------------------------------------------------

write.csv(adjacency_matrix, "Data/Calculated_Adjacency_Matrix_Tracts_2010.csv")
write.csv(adjacency_matrix_v2, "Data/Calculated_Adjacency_Matrix_v2_Tracts_2010.csv")

write.csv(adjacency_list, "Data/Calculated_Adjacency_List_Tracts_2010.csv")
write.csv(adjacency_list_v2, "Data/Calculated_Adjacency_List_v2_Tracts_2010.csv")

write.csv(distance_matrix, "Data/Calculated_Distance_Matrix_Tracts_2010.csv")
write.csv(distance_matrix_v2, "Data/Calculated_Distance_Matrix_v2_Tracts_2010.csv")



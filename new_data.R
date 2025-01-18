
# https://web.stanford.edu/~jrodden/OH_release/
geo <- read.csv("Data/oh_geo_merged_final.csv")
vote <- read.csv("Data/oh_vote_merged_final.csv")
output <- read.csv("District Outputs Updated/output_vtd01.csv")
map <- sf::read_sf(dsn = "Data/Shapes & other/OH_final.shp")

votecombo <- dplyr::full_join(vote,output,dplyr::join_by("GEOID10"=="Geography")) %>%
  dplyr::mutate(missing_data_flag = is.na(total_08)) %>%
  dplyr::group_by(district,missing_data_flag) %>%
  dplyr::summarise(
    reg_08 = sum(reg_08, na.rm = TRUE),
    total_08 = sum(total_08, na.rm = TRUE),
    pres_dvote_08 = sum(pres_dvote08, na.rm = TRUE),
    pres_rvote_08 = sum(pres_rvote08, na.rm = TRUE),
    pres_tvote_08 = sum(pres_tvote_08, na.rm = TRUE),
    two_party_vote = pres_dvote_08 + pres_rvote_08
  )

# adjaceny matrix

tractlist <- unique(map$GEOID10)

map <- map %>%
  tibble::rowid_to_column(var = "rowid")


neighbor_list <- spdep::poly2nb(map)

# calculate adjacency matrix
tract_adjacency <- spdep::nb2mat(neighbor_list, style = "B") %>%
  as.data.frame()
# add names to columns (row names already present)
names(tract_adjacency) <- tractlist
tract_adjacency$SOURCE_TRACTID <- tractlist

# calculate adjacency list
tract_adj_list <- tract_adjacency %>%
  dplyr::relocate(SOURCE_TRACTID, .before = 1) %>%
  tidyr::pivot_longer(2:11030, names_to = "NEIGHBOR_TRACTID", values_to = "value") %>%
  dplyr::filter(value == 1) %>%
  dplyr::select(-value)

isContig(unique(tract_adj_list$SOURCE_TRACTID), tract_adj_list)

tract_adj_count <- tract_adj_list %>%
  dplyr::group_by(SOURCE_TRACTID) %>%
  dplyr::tally() %>%
  dplyr::ungroup()




# mapview(map)

write.csv(tract_adj_list,"Data/tract_adj_list_rodden.csv", row.names = FALSE)

library(igraph)
library(Matrix)
library(e1071)
library(netmeta)
library(tidyverse)

adj_matrix <- read.csv("Data/tract_adj_list_rodden.csv") %>%
  dplyr::mutate(adj = 1) %>%
  tidyr::pivot_wider(names_from = "NEIGHBOR_TRACTID", values_from = "adj") %>%
  tibble::column_to_rownames(var = "SOURCE_TRACTID")

adj_matrix_small <- adj_matrix[c(1:100),c(1:100)]

adj_matrix_small[is.na(adj_matrix_small)] <- 0
adj_matrix[is.na(adj_matrix)] <- 0

adj_matrix_small <- as.matrix(adj_matrix_small)
adj_matrix <- as.matrix(adj_matrix)


g1 <- igraph::graph_from_adjacency_matrix(adj_matrix, weighted=FALSE, diag = TRUE)

test <- e1071::allShortestPaths(adj_matrix)
test <- netmeta::netdistance(adj_matrix)

table(test$length)


# This script uses the US Census Bureau and TIGER/Line 2010 voting district shapefiles and
# creates district adjacency files

### load libraries ----------------------------------------------------------------------
library(sf)
library(mapview)
library(spdep)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------
# file path as a string to the folder containing the downloaded shapefiles.
base_path <- "Data/Ohio 2010s Voting Districts"

# create a list of files within the above folder
base_path_list <- list.files(base_path)

# for each file in the folder, load the shapefile within and rename the imported
# data to the name of the file
for(folder in base_path_list){
  
  file_path <- paste0(base_path,"/",folder,"/",folder,".shp")
  df <- sf::read_sf(dsn = file_path)
  assign(folder,df)
  
}

# bind all files with the below pattern (TIGER/Line, year 2010, Ohio FIPS)
ohio_vtd_shapefile <- mget(ls(pattern="tl_2010_39")) %>%
  bind_rows()

# add GEOID variable as row names
row.names(ohio_vtd_shapefile) <- ohio_vtd_shapefile$GEOID10

# convert to an sf object
ohio_vtd_shapefile <- sf::st_as_sf(ohio_vtd_shapefile)
sf::write_sf(ohio_vtd_shapefile, dsn = "Data/ohio_vtd_shapefile.shp")

# plot(ohio_vtd_shapefile)

vtd_list <- unique(ohio_vtd_shapefile$GEOID10)

neighborhood_list <- spdep::poly2nb(ohio_vtd_shapefile)

# calculate adjacency matrix
vtd_adjacency <- spdep::nb2mat(neighborhood_list, style = "B") %>%
  as.data.frame()
# add names to columns (row names already present)
names(vtd_adjacency) <- vtd_list

# calculate adjacency list
vtd_adj_list <- vtd_adjacency %>%
  tibble::rownames_to_column(var = "SOURCE_TRACTID") %>%
  tidyr::pivot_longer(2:11030, names_to = "NEIGHBOR_TRACTID", values_to = "value") %>%
  dplyr::filter(value == 1) %>%
  dplyr::select(-value)

isContig(vtd_adj_list)

vtd_adj_count <- vtd_adj_list %>%
  dplyr::group_by(SOURCE_TRACTID,value) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(value = as.character(value)) %>%
  tidyr::pivot_wider(names_from = "value", values_from = "n")

vtd_adj1 <- vtd_adj_count %>%
  dplyr::filter(`1` == 1) %>%
  dplyr::pull(SOURCE_TRACTID)

shape_adj1 <- ohio_vtd_shapefile %>%
  dplyr::mutate(color = dplyr::case_when(
    GEOID10 %in% vtd_adj1 ~ "red",
    .default = "lightblue"
  ))

mapview(shape_adj1, zcol = "color")

write.csv(vtd_adj_list,"Data/vtd_adj_list.csv", row.names = FALSE)

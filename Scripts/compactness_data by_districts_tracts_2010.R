
# This script takes an output from the randomDistance file, adds voting data, and
# calculates district-wide voting partisanship.

### load libraries ----------------------------------------------------------------------

library(readxl)
library(tibble)
library(sf)
library(mapview)
library(lwgeom)
library(tidyverse)

### 'not in' function ----------------------------------------------------------------------

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


## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10)


## Output Data
# The output CSV file from the random districts tracts 2010 script
output <- read.csv("District Outputs Tracts 2010/output10.csv",
                   colClass = "character") %>%
  dplyr::select(Geography,district)

### format data ----------------------------------------------------------------------

area <- shape_tract %>%
  dplyr::left_join(output,
                   by = "Geography") %>%
  sf::st_as_sf()


for(a in c(1:16)){
  
  tmp <- area %>%
    dplyr::filter(district == a) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(district = a, .before = 1) %>%
    dplyr::rename(geometry = 2)
  
  assign(paste0("district_shape_",a),tmp)
  
}

district_map_shapefile <- mget(ls(pattern="district_shape_")) %>%
  bind_rows()

rm(list=ls(pattern="^district_shape_"))

compactness_by_district <- district_map_shapefile %>%
  # calculate statistics / compute compactness measurements
  dplyr::mutate(
    District = factor(district,
                      levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
                                 "10", "11", "12", "13", "14", "15", "16")),
    `District Perimeter` = sf::st_perimeter(geometry),
    `District Area` = sf::st_area(geometry),
    # Polsby-Popper compactness score
    # The Polsby-Popper (PP) measure (Polsby & Popper, 1991) is the ratio of the area
    # of the district to the area of a circle whose circumference is equal to the perimeter
    # of the district. A district’s Polsby-Popper score falls with the range of [0,1] and a
    # score closer to 1 indicates a more compact district.
    `Compactness Polsby Popper` = 4 * pi * `District Area` / (`District Perimeter`^2),
    `Compactness Polsby Popper` = as.numeric(`Compactness Polsby Popper`),
    # Schwartzberg compactness score
    # The Schwartzberg score compactness score is the ratio of the perimeter of the district to
    # the circumference of a circle whose area is equal to the area of the district. A district’s
    # Schwartzberg score as calculated below falls with the range of [0,1] and a score closer to 1
    # indicates a more compact district.
    `Compactness Schwartzberg` = 1 / (`District Perimeter` / (2 * pi * sqrt(`District Area` / pi))),
    `Compactness Schwartzberg` = as.numeric(`Compactness Schwartzberg`)
  ) %>%
  as.data.frame() %>%
  dplyr::select(-district, -geometry)

# mapview(compactness_by_district)

write.csv(compactness_by_district, "Districts by Compactness Tracts 2010/compactness_tracts10.csv", row.names = FALSE)

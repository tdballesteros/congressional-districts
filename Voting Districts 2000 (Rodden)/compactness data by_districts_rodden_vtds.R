
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

# adjacency matrix
adj <- read.csv("Data/Tract_adj_list_rodden.csv",
                colClasses = "character")

# Ohio shapefile
area <- sf::read_sf(
  dsn = "Data/Shapes & other/OH_final.shp") %>%
  dplyr::rename_with(tolower)

## Output Data
# The output CSV file from the random_distance_vtds script
output <- read.csv("District Outputs Rodden/output_vtd01.csv",
                   colClass = "character")

### format data ----------------------------------------------------------------------

adj <- adj %>%
  # recode a typo
  dplyr::mutate(
    SOURCE_TRACTID = ifelse(SOURCE_TRACTID == "3906935AAH", "39069035AAH", SOURCE_TRACTID),
    NEIGHBOR_TRACTID = ifelse(NEIGHBOR_TRACTID == "3906935AAH", "39069035AAH", NEIGHBOR_TRACTID)
  )

area <- area %>%
  dplyr::left_join(output,
                   dplyr::join_by("geoid10" == "GEO_ID")) %>%
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
    # Schwartzberg compactness score
    # The Schwartzberg score compactness score is the ratio of the perimeter of the district to
    # the circumference of a circle whose area is equal to the area of the district. A district’s
    # Schwartzberg score as calculated below falls with the range of [0,1] and a score closer to 1
    # indicates a more compact district.
    `Compactness Schwartzberg` = 1 / (`District Perimeter` / (2 * pi * sqrt(`District Area` / pi)))
  ) %>%
  as.data.frame() %>%
  dplyr::select(-district, -geometry)

# mapview(compactness_by_district)

write.csv(compactness_by_district, "Districts by Compactness Rodden/compactness_vtd04.csv", row.names = FALSE)


# This script takes an output from the randomDistance file and calculates district compactness metrics.


### load libraries ----------------------------------------------------------------------

library(tibble)
library(sf)
library(mapview)
library(lwgeom)
library(tidyverse)


### 'not in' function ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

# two-digit code for the output file number
output_number <- "02"

# assign folder filepaths
input_folder <- "Tracts 2010 (alg2)/99_Export Data/District Outputs Tracts 2010 v2"
output_folder <- "Tracts 2010 (alg2)/99_Export Data/Districts by Compactness v2"

## Adjacency List
# Source: Derived from US Census Bureau and TIGER/Line
adjacency_list <- read.csv(
  "Data/Calculated_Adjacency_List_Tracts_2010.csv",
  colClasses = "character"
)

# v2 removes six unpopulated tracts in Lake Erie
adjacency_list_v2 <- read.csv(
  "Data/Calculated_Adjacency_List_v2_Tracts_2010.csv",
  colClasses = "character"
)

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10)

## Output Data
# The output CSV file from the random districts tracts 2010 script
output <- read.csv(paste0(input_folder,"/output",output_number,".csv"),
                   colClass = "character") %>%
  dplyr::select(Geography,district)

## Erie Lake Shapefile
# Source: US Geological Survey
shape_erie_lake <- sf::read_sf("Data/hydro_p_LakeErie/hydro_p_LakeErie.shp") %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  sf::st_sf(crs = "NAD83")


### format shapefile ----------------------------------------------------------------------
# temporary fix - use Erie Lake shapefile to remove water from the census tract shapefile
shape_tract <- shape_tract %>%
  sf::st_difference(shape_erie_lake)


### format data ----------------------------------------------------------------------

run <- c(42:60) %>%
  stringr::str_pad(width = 2, side = "left", pad = "0")

for(output_number in run){


area <- shape_tract %>%
  dplyr::left_join(output, by = "Geography") %>%
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
                      levels = c(1:16)),
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
    `Compactness Schwartzberg` = as.numeric(`Compactness Schwartzberg`),
    # Reock compactness score
    # The Reock Score is the ratio of the area of the district to the area of a minimum bounding cirle
    # that encloses the district’s geometry. A district’s Reock score falls within the range of [0,1] and
    # a score closer to 1 indicates a more compact district.
    minimum_bounding_circle = lwgeom::st_minimum_bounding_circle(geometry),
    mbc_area = sf::st_area(minimum_bounding_circle),
    `Compactness Reock` = `District Area` / mbc_area,
    `Compactness Reock` = as.numeric(`Compactness Reock`)
  ) %>%
  as.data.frame() %>%
  dplyr::select(-c(district, geometry, minimum_bounding_circle, mbc_area)) %>%
  dplyr::arrange(District)

# mapview(compactness_by_district)

write.csv(compactness_by_district,
          paste0(output_folder,"/compactness_tracts",output_number,".csv"),
          row.names = FALSE
          )
}

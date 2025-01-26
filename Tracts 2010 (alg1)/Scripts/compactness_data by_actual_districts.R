
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

ohio_108th_shapefile <- sf::read_sf(
  dsn = "Data/tl_2010_39_cd108/tl_2010_39_cd108.shp"
)

ohio_111th_shapefile <- sf::read_sf(
  dsn = "Data/tl_2010_39_cd111/tl_2010_39_cd111.shp"
)

ohio_112th_shapefile <- sf::read_sf(
  dsn = "Data/tl_2011_us_cd112/tl_2011_us_cd112.shp"
)

ohio_113th_shapefile <- sf::read_sf(
  dsn = "Data/tl_2013_us_cd113/tl_2013_us_cd113.shp"
)

### format data ----------------------------------------------------------------------
  
ohio_108th_shapefile <- ohio_108th_shapefile %>%
  dplyr::select(CDSESSN, NAMELSAD00, geometry) %>%
  dplyr::rename(NAMELSAD = NAMELSAD00)

ohio_111th_shapefile <- ohio_111th_shapefile %>%
  dplyr::select(CDSESSN, NAMELSAD10, geometry) %>%
  dplyr::rename(NAMELSAD = NAMELSAD10)

ohio_112th_shapefile <- ohio_112th_shapefile %>%
  dplyr::filter(STATEFP == 39) %>%
  dplyr::select(CDSESSN, NAMELSAD, geometry)

ohio_113th_shapefile <- ohio_113th_shapefile %>%
  dplyr::filter(STATEFP == 39) %>%
  dplyr::select(CDSESSN, NAMELSAD, geometry)

ohio_congressional_shapefiles <- rbind(ohio_108th_shapefile,
                                       ohio_111th_shapefile,
                                       ohio_112th_shapefile,
                                       ohio_113th_shapefile) %>%
  dplyr::mutate(
    NAMELSAD = factor(NAMELSAD,
                      levels = c("Congressional District 1","Congressional District 2",
                                 "Congressional District 3","Congressional District 4",
                                 "Congressional District 5","Congressional District 6",
                                 "Congressional District 7","Congressional District 8",
                                 "Congressional District 9","Congressional District 10",
                                 "Congressional District 11","Congressional District 12",
                                 "Congressional District 13","Congressional District 14",
                                 "Congressional District 15","Congressional District 16",
                                 "Congressional District 17","Congressional District 18",
                                 "Ohio Average")),
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
    )
  



write.csv(compactness_by_district, "compactness_official_districts.csv", row.names = FALSE)



# This script replicates the measures of compactness for the actual congressional districts.

# NOTE: 108th - 111th congress have 18 representatives from Ohio, 112th - 113th congress have 16.

### load libraries ----------------------------------------------------------------------

library(readxl)
library(tibble)
library(sf)
library(sfheaders)
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

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10)

## Erie Lake Shapefile
# Source: US Geological Survey
shape_erie_lake <- sf::read_sf("Data/hydro_p_LakeErie/hydro_p_LakeErie.shp") %>%
  sf::st_union() %>%
  sf::st_as_sf() %>%
  sf::st_sf(crs = "NAD83")


### format shapefile ----------------------------------------------------------------------

shape_tract <- shape_tract %>%
  sf::st_difference(shape_erie_lake)

ohio_108th_shapefile <- ohio_108th_shapefile %>%
  dplyr::select(CDSESSN, NAMELSAD00, geometry) %>%
  dplyr::rename(NAMELSAD = NAMELSAD00) %>%
  sf::st_difference(shape_erie_lake)

ohio_111th_shapefile <- ohio_111th_shapefile %>%
  dplyr::select(CDSESSN, NAMELSAD10, geometry) %>%
  dplyr::rename(NAMELSAD = NAMELSAD10) %>%
  sf::st_difference(shape_erie_lake)

ohio_112th_shapefile <- ohio_112th_shapefile %>%
  dplyr::filter(STATEFP == 39) %>%
  dplyr::select(CDSESSN, NAMELSAD, geometry) %>%
  sf::st_difference(shape_erie_lake)

ohio_113th_shapefile <- ohio_113th_shapefile %>%
  dplyr::filter(STATEFP == 39) %>%
  dplyr::select(CDSESSN, NAMELSAD, geometry) %>%
  sf::st_difference(shape_erie_lake)
  
# # create an outline of Ohio removing most of Lake Erie
# # this will be applied to the congressional and tract shapefiles to ensure equal inclusion of area
# outline_shapefile_initial <- ohio_108th_shapefile %>%
#   dplyr::filter(CD108FP %in% c("05","09")) %>%
#   sf::st_as_sf() %>%
#   sf::st_cast("POLYGON") %>%
#   sfheaders::sf_remove_holes() %>%
#   sf::st_union() %>%
#   sf::st_as_sf()
# 
# outline_shapefile <- ohio_108th_shapefile %>%
#   dplyr::filter(CD108FP %!in% c("05","09")) %>%
#   sf::st_union() %>%
#   sf::st_as_sf() %>%
#   rbind(outline_shapefile_initial) %>%
#   sf::st_union() %>%
#   sf::st_as_sf()

ohio112 <- ggplot2::ggplot(data = ohio_112th_shapefile,
                       ggplot2::aes(fill = NAMELSAD)) +
  ggplot2::geom_sf() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position="none")

ohio113 <- ggplot2::ggplot(data = ohio_113th_shapefile,
                           ggplot2::aes(fill = NAMELSAD)) +
  ggplot2::geom_sf() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position="none")


gridExtra::grid.arrange(ohio112, ohio113, ncol = 2)



### format data ----------------------------------------------------------------------

compactness_by_district_actual <- rbind(ohio_108th_shapefile,
                                        ohio_111th_shapefile,
                                        ohio_112th_shapefile,
                                        ohio_113th_shapefile) %>%
  # sf::sf_use_s2(FALSE) %>%
  dplyr::mutate(
    geometry = sf::st_collection_extract(geometry),
    NAMELSAD = factor(NAMELSAD,
                      levels = c("Congressional District 1", "Congressional District 2",
                                 "Congressional District 3", "Congressional District 4",
                                 "Congressional District 5", "Congressional District 6",
                                 "Congressional District 7", "Congressional District 8",
                                 "Congressional District 9", "Congressional District 10",
                                 "Congressional District 11", "Congressional District 12",
                                 "Congressional District 13", "Congressional District 14",
                                 "Congressional District 15", "Congressional District 16",
                                 "Congressional District 17", "Congressional District 18",
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
  dplyr::select(-c(geometry, minimum_bounding_circle, mbc_area)) %>%
  dplyr::rename(
    `Congressional Session` = CDSESSN,
    District = NAMELSAD
    )


write.csv(compactness_by_district_actual, "Tracts 2010 (alg2)/99_Export Data/compactness_official_districts.csv", row.names = FALSE)


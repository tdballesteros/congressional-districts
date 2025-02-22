
# Assign 2010 census tract data to 113th congress map

# load data -------------------------------------------------------------------

ohio_113th_shapefile <- sf::read_sf(
  dsn = "Data/tl_2013_us_cd113/tl_2013_us_cd113.shp"
)

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10)

# format data -------------------------------------------------------------------

ohio_113th_shapefile <- ohio_113th_shapefile %>%
  dplyr::filter(STATEFP == 39) %>%
  dplyr::select(CDSESSN, NAMELSAD, geometry)

shape_tract <- shape_tract %>%
  dplyr::mutate(total_tract_area = sf::st_area(geometry))

ohio_113_tract_list <- data.frame()

for(d in c(1:16)){
  
  district_shape <- ohio_113th_shapefile %>%
    dplyr::mutate(District = trimws(stringr::str_sub(NAMELSAD, -2), which = "both", whitespace = "[ \\t\\r\\n\\h]")) %>%
    dplyr::filter(District == d)
  
  tracts <- shape_tract %>%
    sf::st_intersection(district_shape) %>%
    dplyr::select(Geography, total_tract_area, District) %>%
    dplyr::mutate(
      portion_tract_area = sf::st_area(geometry),
      perc_tract_area = portion_tract_area / total_tract_area
      )
  
  ohio_113_tract_list <- rbind(ohio_113_tract_list, tracts)

}

ohio_113th_assigned_tracts <- ohio_113_tract_list %>%
  dplyr::group_by(Geography) %>%
  dplyr::filter(perc_tract_area == max(perc_tract_area)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(perc_tract_area)

# coverage
sum(ohio_113th_assigned_tracts$perc_tract_area)/nrow(ohio_113th_assigned_tracts)

# format only needed columns and export
ohio_113th_assigned_tracts <- ohio_113th_assigned_tracts %>%
  as.data.frame() %>%
  dplyr::select(Geography, District)

write.csv(ohio_113th_assigned_tracts,"Tracts 2010 (alg2)/99_Export Data/ohio_113th_assigned_tracts.csv",row.names = FALSE)


# This script compiles Random District Outputs - Tracts 2010 Race data

### load libraries ----------------------------------------------------------------------
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------
## Population Data
# Source: US Census Bureau
population_data <- read.csv(
  "Data/population_data_2010_by_tract.csv",
  skip = 1,
  colClasses = "character"
)

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract_v2 <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10) %>%
  dplyr::filter(Geography %!in% c("39095990000", "39043990100", "39093990200",
                                  "39035990000", "39085990000", "39007990000"))

## Output Data
# The output CSV file from the random districts tracts 2010 script
output <- read.csv("Tracts 2010 (alg2)/Export Data/District Outputs Tracts 2010/output02.csv",
                   colClass = "character") %>%
  dplyr::select(Geography, district)


### format data ----------------------------------------------------------------------

# format race data
pop_tracts <- population_data %>%
  dplyr::filter(
    # Ohio FIPS code is 39
    stringr::str_sub(Geography, 10, 11) == "39",
    # filter out total row for Ohio
    Geography != "0400000US39",
    # filter out empty Lake Erie tracts
    Geography %!in% c("39095990000", "39043990100", "39093990200",
                      "39035990000", "39085990000", "39007990000")
  ) %>%
  tidyr::pivot_longer(3:73, names_to = "Race", values_to = "Population") %>%
  dplyr::select(-X) %>%
  dplyr::filter(Race %!in% c(
    "Total..Population.of.one.race",
    "Total..Two.or.More.Races",
    "Total..Two.or.More.Races..Population.of.six.races",
    "Total..Two.or.More.Races..Population.of.five.races",
    "Total..Two.or.More.Races..Population.of.four.races",
    "Total..Two.or.More.Races..Population.of.three.races",
    "Total..Two.or.More.Races..Population.of.two.races"
  )) %>%
  dplyr::mutate(
    Geography = stringr::str_sub(Geography, 10),
    # recode race variable names and consolidate multirace into one category
    Race = dplyr::case_when(
      Race == "Total" ~ "Population",
      Race == "Total..Population.of.one.race..American.Indian.and.Alaska.Native.alone" ~ "AIAN",
      Race == "Total..Population.of.one.race..Asian.alone" ~ "Asian",
      Race == "Total..Population.of.one.race..Black.or.African.American.alone" ~ "Black",
      Race == "Total..Population.of.one.race..Native.Hawaiian.and.Other.Pacific.Islander.alone" ~ "NHPI",
      Race == "Total..Population.of.one.race..Some.Other.Race.alone" ~ "Other",
      Race == "Total..Population.of.one.race..White.alone" ~ "White",
      .default = "Two or More Races"
    ),
    # several tracts have non-numeric entries in the Population  field; manually correct these
    # and convert all data to numeric values
    Population = dplyr::case_when(
      Geography %in% c("39035118800", "39035141300", "39035187105", "39035187106", "39035195900", "39055310200",
                       "39055310600", "39055310800", "39055310900", "39055311000", "39055311300", "39055311400",
                       "39055311600", "39055311700", "39055311800", "39055312100", "39055312202", "39055312300",
                       "39055312400", "39085206400", "39155930500") &
        Race == "Population" ~ as.numeric(stringr::str_sub(Population, 1, 4)),
      .default = as.numeric(Population)
    )) %>%
  # sum Population over Geography & Race
  dplyr::group_by(Geography, Race) %>%
  dplyr::summarise(
    Population = sum(Population, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = Race, values_from = Population) %>%
  # reorder columns
  dplyr::select(Geography,Population, White, Black, Asian,
                NHPI, AIAN, `Two or More Races`, Other) %>%
  dplyr::mutate(
    `White Percentage` = White / Population,
    `Black Percentage` = Black / Population,
    `Asian Percentage` = Asian / Population,
    `NHPI Percentage` = NHPI / Population,
    `AIAN Percentage` = AIAN / Population,
    `Two or More Races Percentage` = `Two or More Races` / Population,
    `Other Percentage` = Other / Population,
    `Non-White Percentage` = 1 - `White Percentage`
  )

# format output data as a shapefile
output_shapefile <- shape_tract_v2 %>%
  dplyr::left_join(output, by = "Geography") %>%
  sf::st_as_sf()

for(a in c(1:16)){
  
  tmp <- output_shapefile %>%
    dplyr::filter(district == a) %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(district = a, .before = 1) %>%
    dplyr::rename(geometry = 2)
  
  assign(paste0("district_shape_",a),tmp)
  
}

output_shapefile <- mget(ls(pattern="district_shape_")) %>%
  bind_rows() %>%
  dplyr::mutate(district = factor(district, c(1:16)))

mapview(output_shapefile)

rm(list=ls(pattern="^district_shape_"))


st_crs(output_shapefile)

# format race data as a shapefile
race_map_dataset <- dplyr::full_join(shape_tract_v2,
                                     pop_tracts,
                                     by = "Geography")

# create map
map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = race_map_dataset,
                   ggplot2::aes(fill = `Non-White Percentage`), lwd = 0) +
  ggplot2::scale_fill_distiller(palette = "YlOrRd") +
  ggplot2::geom_sf(data = output_shapefile, fill = NA, color = "#000000")

# map <- ggplot2::ggplot(data = district_map_shapefile,
#                        ggplot2::aes(fill = District)) +
#   ggplot2::geom_sf() +
#   ggplot2::scale_fill_manual(values = color_palette) +
#   ggplot2::theme_void()

map


mapview(race_map_dataset, zcol = "Non-White Perc")



color_palette <- c("dodgerblue3","firebrick2","chartreuse2","darkorchid3","darkslategray4",
                   "orange2","lightsalmon","hotpink1","turquoise","darkseagreen2","lightblue3",
                   "mediumorchid1","plum","ivory2","goldenrod","olivedrab3")

map <- pop_final %>%
  dplyr::left_join(shape_tract_v2,
                   by = "Geography")

for(a in c(1:16)){
  
  tmp <- map %>%
    dplyr::filter(district == a) %>%
    sf::st_as_sf() %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(district = a, .before = 1) %>%
    dplyr::rename(geometry = 2)
  
  assign(paste0("district_shape_",a),tmp)
  
}

district_map_shapefile <- mget(ls(pattern="district_shape_")) %>%
  bind_rows() %>%
  dplyr::mutate(District = factor(district, c(1:16))) %>%
  dplyr::select(-district)

map <- ggplot2::ggplot(data = district_map_shapefile,
                       ggplot2::aes(fill = District)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_manual(values = color_palette) +
  ggplot2::theme_void()

map


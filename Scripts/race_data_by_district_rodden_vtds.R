
# This script takes an output from the randomDistance file, adds race/ethnicity data, and
# calculates district-wide racial composition.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------
# vtd list with populations
oh2010 <- read.csv("Data/population_data_2010_by_vtd.csv",
                   skip = 1)

## Output Data
# The output CSV file from the random_distance_vtds script
output <- read.csv("District Outputs Rodden/output_vtd01.csv",
                   colClass = "character")

### format data ----------------------------------------------------------------------

pop_vtds <- oh2010 %>%
  tidyr::pivot_longer(3:73, names_to = "Race", values_to = "Population") %>%
  dplyr::select(-X) %>%
  dplyr::filter(Race %!in% c("Total..Population.of.one.race",
                             "Total..Two.or.More.Races",
                             "Total..Two.or.More.Races..Population.of.six.races",
                             "Total..Two.or.More.Races..Population.of.five.races",
                             "Total..Two.or.More.Races..Population.of.four.races",
                             "Total..Two.or.More.Races..Population.of.three.races",
                             "Total..Two.or.More.Races..Population.of.two.races")) %>%
  # recode race variable names and consolidate multirace into one category
  dplyr::mutate(
    Population = as.numeric(Population),
    Race = dplyr::case_when(
      Race == "Total" ~ "Population",
      Race == "Total..Population.of.one.race..American.Indian.and.Alaska.Native.alone" ~ "AIAN",
      Race == "Total..Population.of.one.race..Asian.alone" ~ "Asian",
      Race == "Total..Population.of.one.race..Black.or.African.American.alone" ~ "Black",
      Race == "Total..Population.of.one.race..Native.Hawaiian.and.Other.Pacific.Islander.alone" ~ "NHPI",
      Race == "Total..Population.of.one.race..Some.Other.Race.alone" ~ "Other",
      Race == "Total..Population.of.one.race..White.alone" ~ "White",
      .default = "Two or More Races"
    )) %>%
  # sum Population over Geography & Race
  dplyr::group_by(Geography, Race) %>%
  dplyr::summarise(
    Population = sum(Population, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    fips_county = stringr::str_sub(Geography, 12, 14),
    fips_tract = stringr::str_sub(Geography, 15, 20),
    Geography = stringr::str_sub(Geography, 10)
  ) %>%
  tidyr::pivot_wider(names_from = Race, values_from = Population) %>%
  # reorder columns
  dplyr::select(Geography, fips_county, fips_tract, Population, White, Black, Asian,
                NHPI, AIAN, `Two or More Races`, Other)

### race data ----------------------------------------------------------------------
districts_race_data <- dplyr::full_join(output, pop_vtds, dplyr::join_by("GEO_ID" == "Geography")) %>%
  dplyr::mutate(missing_race_data_flag = is.na(Population)) %>%
  dplyr::group_by(district, missing_race_data_flag) %>%
  dplyr::summarise(
    Population = sum(Population, na.rm = TRUE),
    `White %` = 100 * sum(White, na.rm = TRUE) / Population,
    `Black %` = 100 * sum(Black, na.rm = TRUE) / Population,
    `Asian %` = 100 * sum(Asian, na.rm = TRUE) / Population,
    `NHPI %` = 100 * sum(NHPI, na.rm = TRUE) / Population,
    `AIAN %` = 100 * sum(AIAN, na.rm = TRUE) / Population,
    `Two or More Races %` = 100 * sum(`Two or More Races`, na.rm = TRUE) / Population,
    `Other %` = 100 * sum(Other, na.rm = TRUE) / Population,
    `Non-White %` = 100 - `White %`
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(district = as.numeric(district)) %>%
  dplyr::arrange(district)


### population sizes ----------------------------------------------------------------------
district_target_population <- sum(pop_vtds$Population, na.rm = TRUE) / 16

districts_population_data <- districts_race_data %>%
  dplyr::filter(
    missing_race_data_flag == 0,
    !is.na(district)
    ) %>%
  dplyr::select(district, Population)

# calculates the size of the largest district by population relative to the target population
max_district_perc <- 100 * max(districts_population_data$Population, na.rm = TRUE) / district_target_population


# calculates the size of the smallest district by population relative to the target population
min_district_perc <- 100 * min(districts_population_data$Population, na.rm = TRUE) / district_target_population


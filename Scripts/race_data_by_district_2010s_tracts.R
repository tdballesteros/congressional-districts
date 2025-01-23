
# This script takes an output from the randomDistance file, adds race/ethnicity data, and
# calculates district-wide racial composition.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------
## Population Data
# Source: US Census Bureau
# tract data
tract_data <- read.csv("Data/population_data_2010_by_tract.csv",
                       skip = 1,
                       colClasses = "character")

## Output Data
# The output CSV file from the raceData script
output <- read.csv("District Outputs Tracts 2010/output10.csv",
                   colClass = "character") %>%
  dplyr::select(Geography,district)

### format data ----------------------------------------------------------------------
pop_tracts <- tract_data %>%
  dplyr::filter(
    # Ohio FIPS code is 39
    stringr::str_sub(Geography, 10, 11) == "39",
    # filter out total row for Ohio
    Geography != "0400000US39"
  ) %>%
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
      Geography == "1400000US39035118800" & Race == "Population" ~ 3081,
      Geography == "1400000US39035141300" & Race == "Population" ~ 2661,
      Geography == "1400000US39035187105" & Race == "Population" ~ 2176,
      Geography == "1400000US39035187106" & Race == "Population" ~ 5198,
      Geography == "1400000US39035195900" & Race == "Population" ~ 4233,
      Geography == "1400000US39055310200" & Race == "Population" ~ 1991,
      Geography == "1400000US39055310600" & Race == "Population" ~ 6148,
      Geography == "1400000US39055310800" & Race == "Population" ~ 6621,
      Geography == "1400000US39055310900" & Race == "Population" ~ 3200,
      Geography == "1400000US39055311000" & Race == "Population" ~ 3637,
      Geography == "1400000US39055311300" & Race == "Population" ~ 4412,
      Geography == "1400000US39055311400" & Race == "Population" ~ 5537,
      Geography == "1400000US39055311600" & Race == "Population" ~ 3810,
      Geography == "1400000US39055311700" & Race == "Population" ~ 4089,
      Geography == "1400000US39055311800" & Race == "Population" ~ 7306,
      Geography == "1400000US39055312100" & Race == "Population" ~ 4131,
      Geography == "1400000US39055312202" & Race == "Population" ~ 4323,
      Geography == "1400000US39055312300" & Race == "Population" ~ 4643,
      Geography == "1400000US39055312400" & Race == "Population" ~ 2544,
      Geography == "1400000US39085206400" & Race == "Population" ~ 4701,
      Geography == "1400000US39155930500" & Race == "Population" ~ 6115,
      .default = as.numeric(Population)
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

district_target_population <- sum(pop_tracts$Population, na.rm = TRUE) / 16


### race data ----------------------------------------------------------------------
districts_race_data <- dplyr::full_join(output, pop_tracts, by = "Geography") %>%
  dplyr::mutate(District = factor(district,
                                  levels = c(1:16, "Ohio Total"))) %>%
  dplyr::group_by(District) %>%
  dplyr::summarise(
    Population = sum(Population, na.rm = TRUE),
    `White %` = 100 * sum(White, na.rm = TRUE) / Population,
    `Black %` = 100 * sum(Black, na.rm = TRUE) / Population,
    `Asian %` = 100 * sum(Asian, na.rm = TRUE) / Population,
    `NHPI %` = 100 * sum(NHPI, na.rm = TRUE) / Population,
    `AIAN %` = 100 * sum(AIAN, na.rm = TRUE) / Population,
    `Two or More Races %` = 100 * sum(`Two or More Races`, na.rm = TRUE) / Population,
    `Other %` = 100 * sum(Other, na.rm = TRUE) / Population,
    `Non-White %` = 100 - `White %`,
  ) %>%
  dplyr::ungroup() %>%
  # add Ohio total row
  tibble::add_row(
    District = "Ohio Total",
    Population = sum(pop_tracts$Population, na.rm = TRUE),
    `White %` = 100 *sum(pop_tracts$White, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Black %` = 100 *sum(pop_tracts$Black, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Asian %` = 100 *sum(pop_tracts$Asian, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `NHPI %` = 100 *sum(pop_tracts$NHPI, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `AIAN %` = 100 *sum(pop_tracts$AIAN, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Two or More Races %` = 100 *sum(pop_tracts$`Two or More Races`, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Other %` = 100 *sum(pop_tracts$Other, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Non-White %` = 100 - `White %`
  ) %>%
  # add Race Score, where % white - % non-white is classified based on:
  # < 0 (majority-minority) - 0
  # [0, 45) - 1
  # [45, 60) - 2
  # [60, 70) - 3
  # [70, 85) - 4
  # > 85 - 5
  dplyr::mutate(
    `White % Minus Non-White %` = `White %` - `Non-White %`,
    `Race Score` = dplyr::case_when(
      `White % Minus Non-White %` < 0 ~ 0,
      `White % Minus Non-White %` >= 0 &
        `White % Minus Non-White %` < 45 ~ 1,
      `White % Minus Non-White %` >= 45 &
        `White % Minus Non-White %` < 60 ~ 2,
      `White % Minus Non-White %` >= 60 &
        `White % Minus Non-White %` < 70 ~ 3,
      `White % Minus Non-White %` >= 70 &
        `White % Minus Non-White %` < 85 ~ 4,
      `White % Minus Non-White %` >= 85 ~ 5,
      .default = NA
      ),
    # for districts without missing data, calculate population relative to
    # target population ratio
    `Population Target Ratio` = dplyr::case_when(
      District %in% c(1:16) ~ 100 * Population / district_target_population,
      .default = NA
  ))

write.csv(districts_race_data, "Districts by Race Tracts 2010/race_tracts10.csv", row.names = FALSE)


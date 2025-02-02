
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
population_data <- read.csv(
  "Data/population_data_2010_by_tract.csv",
  skip = 1,
  colClasses = "character"
)

## Output Data
# The output CSV file from the random districts tracts 2010 script
output <- read.csv("Tracts 2010 (alg2)/Export Data/District Outputs Tracts 2010/output30.csv",
                   colClass = "character") %>%
  dplyr::select(Geography, district)

### format data ----------------------------------------------------------------------
pop_tracts <- population_data %>%
  dplyr::filter(
    # Ohio FIPS code is 39
    stringr::str_sub(Geography, 10, 11) == "39",
    # filter out total row for Ohio
    Geography != "0400000US39"
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
  dplyr::mutate(
    fips_county = stringr::str_sub(Geography, 12, 14),
    fips_tract = stringr::str_sub(Geography, 15, 20)
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

write.csv(districts_race_data,
          "Tracts 2010 (alg2)/Export Data/Districts by Race/race_tracts30.csv",
          row.names = FALSE
          )

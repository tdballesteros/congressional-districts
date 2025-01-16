
# This script takes an output from the randomDistance file, adds race/ethnicity data, and
# calculates district-wide racial composition.


### load libraries ----------------------------------------------------------------------

library(tidyverse)


### 'not in' function ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

# tract list with populations
tract_data <- read.csv("Data/BlockGr_Ohio.csv")

# output data
output <- read.csv("output1.csv")


### race data ----------------------------------------------------------------------

race <- tract_data %>%
  # all non-Hispanic or Latino categories are exclusively non-Hispanic
  dplyr::filter(lntitle != "Not Hispanic or Latino") %>%
  dplyr::mutate(
    GEO_ID = str_sub(geoid, 8, 18),
    # consolidate coding for mutliracial people into Two or More Races
    lntitle = dplyr::case_when(
      lntitle == "American Indian or Alaska Native Alone" ~ "AIAN",
      lntitle == "American Indian or Alaska Native and Black or African American" ~ "Two or More Races",
      lntitle == "American Indian or Alaska Native and White" ~ "Two or More Races",
      lntitle == "Asian Alone" ~ "Asian",
      lntitle == "Asian and White" ~ "Two or More Races",
      lntitle == "Black or African American Alone" ~ "Black",
      lntitle == "Black or African American and White" ~ "Two or More Races",
      lntitle == "Native Hawaiian or Other Pacific Islander Alone" ~ "Pacific Islander",
      lntitle == "Remainder of Two or More Race Responses" ~ "Two or More Races",
      lntitle == "Total" ~ "Total",
      lntitle == "White Alone" ~ "White",
      lntitle == "Hispanic or Latino" ~ "Hispanic or Latino",
      .default = lntitle
    ),
    # convert race variable to factor
    lntitle = factor(
      lntitle,
      levels = c("Total","White", "Black", "Hispanic or Latino", "Asian",
                 "AIAN", "Pacific Islander", "Two or More Races")
      )) %>%
  dplyr::select(GEO_ID, lntitle, CIT_EST) %>%
  # group and summarise by tract to remove different entries for blocks
  # and consolidate recoded racial groups
  dplyr::group_by(GEO_ID, lntitle) %>%
  dplyr::summarise(CIT_EST = sum(CIT_EST, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = lntitle, values_from = CIT_EST)


### ethnicity data ----------------------------------------------------------------------

# Note: This dataset omits the "Total" variable as it is carried from the race dataset.

ethnicity <- tract_data %>%
  dplyr::filter(lntitle %in% c("Not Hispanic or Latino",
                               "Hispanic or Latino")) %>%
  dplyr::mutate(
    GEO_ID = str_sub(geoid, 8, 18),
    # convert ethnicity variable to factor
    lntitle = factor(
      lntitle,
      levels = c("Hispanic or Latino", "Not Hispanic or Latino")
    )) %>%
  dplyr::select(GEO_ID, lntitle, CIT_EST) %>%
  # group and summarise by tract to remove different entries for blocks
  dplyr::group_by(GEO_ID, lntitle) %>%
  dplyr::summarise(CIT_EST = sum(CIT_EST, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = lntitle, values_from = CIT_EST)


### full data ----------------------------------------------------------------------

race_by_district <- output %>%
  dplyr::mutate(GEO_ID = as.character(GEO_ID)) %>%
  dplyr::left_join(race, by = "GEO_ID") %>%
  # total tract population estimates do not equal the sum of race estimates, so recalculate race estimates
  # proportionally based on Total (extremes: -1.19% and 1.04% of Total value)
  dplyr::mutate(
    estimate_sum = White + Black + `Hispanic or Latino` + Asian + AIAN + `Pacific Islander` + `Two or More Races`,
    White = Total * White / estimate_sum,
    Black = Total * Black / estimate_sum,
    `Hispanic or Latino` = Total * `Hispanic or Latino` / estimate_sum,
    Asian = Total * Asian / estimate_sum,
    AIAN = Total * AIAN / estimate_sum,
    `Pacific Islander` = Total * `Pacific Islander` / estimate_sum,
    `Two or More Races` = Total * `Two or More Races` / estimate_sum
    ) %>%
  dplyr::select(-c(Total, estimate_sum)) %>%
  # group by district
  dplyr::group_by(district) %>%
  dplyr::summarise(
    CIT_EST = sum(CIT_EST, na.rm = TRUE),
    White = sum(White, na.rm = TRUE),
    Black = sum(Black, na.rm = TRUE),
    `Hispanic or Latino` = sum(`Hispanic or Latino`, na.rm = TRUE),
    Asian = sum(Asian, na.rm = TRUE),
    AIAN = sum(AIAN, na.rm = TRUE),
    `Pacific Islander` = sum(`Pacific Islander`, na.rm = TRUE),
    `Two or More Races` = sum(`Two or More Races`, na.rm = TRUE),
    `White %` = 100 * White / CIT_EST,
    `Black %` = 100 * Black / CIT_EST,
    `Hispanic or Latino %` = 100 * `Hispanic or Latino` / CIT_EST,
    `Asian %` = 100 * Asian / CIT_EST,
    `AIAN %` = 100 * AIAN / CIT_EST,
    `Pacific Islander %` = 100 * `Pacific Islander` / CIT_EST,
    `Two or More Races %` = 100 * `Two or More Races` / CIT_EST,
    `Non-White %` = 100 - `White %`
  )
  



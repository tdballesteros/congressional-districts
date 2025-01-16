
# This script takes an output from the randomDistance file, adds voting data, and
# calculates district-wide voting partisanship.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------
## Population Data
# Source: US Census Bureau
tract_data <- read.csv("Data/population_data_2010_by_tract.csv",
                       skip = 1,
                       colClasses = "character")

## Voting Data
# Source: Redistricting Data Hub (RDH)
# Source Website: https://redistrictingdatahub.org/state/ohio/
voting_data <- read.csv("Data/ohio_2020_election_data_by_block.csv",
                        colClasses = "character")

## Output Data
# The output CSV file from the raceData script
output <- read.csv("District Outputs Updated/output01.csv",
                   colClass = "character") %>%
  dplyr::select(Geography,district)


### format data ----------------------------------------------------------------------
pop_tracts_total <- tract_data %>%
  dplyr::filter(
    # Ohio FIPS code is 39
    stringr::str_sub(Geography, 10, 11) == "39",
    # filter out total row for Ohio
    Geography != "0400000US39"
  ) %>%
  dplyr::select(Geography, Total) %>%
  dplyr::rename(Population = Total) %>%
  # several tracts have non-numeric entries in the Population  field; manually correct these
  # and convert all data to numeric values
  dplyr::mutate(
    Population = dplyr::case_when(
      Geography == "1400000US39035118800" ~ 3081,
      Geography == "1400000US39035141300" ~ 2661,
      Geography == "1400000US39035187105" ~ 2176,
      Geography == "1400000US39035187106" ~ 5198,
      Geography == "1400000US39035195900" ~ 4233,
      Geography == "1400000US39055310200" ~ 1991,
      Geography == "1400000US39055310600" ~ 6148,
      Geography == "1400000US39055310800" ~ 6621,
      Geography == "1400000US39055310900" ~ 3200,
      Geography == "1400000US39055311000" ~ 3637,
      Geography == "1400000US39055311300" ~ 4412,
      Geography == "1400000US39055311400" ~ 5537,
      Geography == "1400000US39055311600" ~ 3810,
      Geography == "1400000US39055311700" ~ 4089,
      Geography == "1400000US39055311800" ~ 7306,
      Geography == "1400000US39055312100" ~ 4131,
      Geography == "1400000US39055312202" ~ 4323,
      Geography == "1400000US39055312300" ~ 4643,
      Geography == "1400000US39055312400" ~ 2544,
      Geography == "1400000US39085206400" ~ 4701,
      Geography == "1400000US39155930500" ~ 6115,
      .default = as.numeric(Population)
    )) %>%
  dplyr::mutate(
    Geography = stringr::str_sub(Geography, 10)
  ) %>%
  # select columns
  dplyr::select(Geography, Population)


pop_vtds_total <- voting_district_data %>%
  # Ohio FIPS code is 39
  dplyr::filter(stringr::str_sub(Geography, 10, 11) == "39") %>%
  dplyr::select(Geography, Total) %>%
  dplyr::rename(Population = Total) %>%
  # convert all data to numeric values
  dplyr::mutate(
    Population = as.numeric(Population),
    Geography = stringr::str_sub(Geography, 10)
  ) %>%
  # select columns
  dplyr::select(Geography, Population)


voting_data_tract <- voting_data %>%
  dplyr::mutate(
    G20PREDBID = as.numeric(G20PREDBID),
    G20PREGHAW = as.numeric(G20PREGHAW),
    G20PRELJOR = as.numeric(G20PRELJOR),
    G20PRERTRU = as.numeric(G20PRERTRU),
    VAP_MOD = as.numeric(VAP_MOD),
    total_votes = G20PREDBID + G20PREGHAW + G20PRELJOR + G20PRERTRU) %>%
  dplyr::group_by(COUNTYFP20, TRACTCE20) %>%
  dplyr::summarise(
    VAP_MOD = sum(VAP_MOD, na.rm = TRUE),
    G20PREDBID = sum(G20PREDBID, na.rm = TRUE),
    G20PREGHAW = sum(G20PREGHAW, na.rm = TRUE),
    G20PRELJOR = sum(G20PRELJOR, na.rm = TRUE),
    G20PRERTRU = sum(G20PRERTRU, na.rm = TRUE),
    total_votes = sum(total_votes, na.rm = TRUE),
    votes_2party = G20PREDBID + G20PRERTRU
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Geography = paste0("39", COUNTYFP20, TRACTCE20)
  )

### district partisanship ----------------------------------------------------------------------
# it appears some tracts in the census data were split by the 2018 elections; consolidate voting
# records for these new tracts into the combined old tract manually
# voting_by_district <- voting_data_tract %>%
#   dplyr::mutate(Geography = dplyr::case_when(
#     Geography %in% c("39001770301", "39001770302") ~ "39001770300"
#   ))



voting_by_district <- dplyr::full_join(output, voting_data_tract, by = "Geography") %>%
  # add census population information
  dplyr::full_join(pop_tracts_total, by = "Geography")

# assess missingness
missing_data <- voting_by_district %>%
  dplyr::mutate(
    voting_data_missing_flag = ifelse(is.na(total_votes), 1, 0),
    census_data_missing_flag = ifelse(is.na(Population), 1, 0),
    district_data_missing_flag = ifelse(is.na(district), 1, 0),
    last_tract_digit = stringr::str_sub(Geography, -1)
  )

table(missing_data$voting_data_missing_flag, useNA = "always")
table(missing_data$census_data_missing_flag, useNA = "always")

voting_minus1 <- missing_data %>%
  dplyr::filter(census_data_missing_flag  == 1) %>%
  dplyr::mutate(Geography_short = stringr::str_sub(Geography, 1, -2)) %>%
  dplyr::select(Geography, Geography_short) %>%
  dplyr::rename(Geography_voting = Geography) %>%
  unique()

census_minus1 <- missing_data %>%
  dplyr::filter(voting_data_missing_flag  == 1) %>%
  dplyr::mutate(Geography_short = stringr::str_sub(Geography, 1, -2)) %>%
  dplyr::select(Geography, Geography_short) %>%
  dplyr::rename(Geography_census = Geography) %>%
  unique()

combined_minus1 <- dplyr::left_join(voting_minus1,
                                    census_minus1,
                                    by = "Geography_short") %>%
  dplyr::filter(!is.na(Geography_census)) %>%
  dplyr::group_by(Geography_voting) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(count == 1) %>%
  select(-count, -Geography_short)

voting_by_district <- voting_data_tract %>%
  # recode voting tracts with matching census tracts
  dplyr::left_join(combined_minus1, dplyr::join_by("Geography" == "Geography_voting")) %>%
  dplyr::mutate(Geography = dplyr::coalesce(Geography_census, Geography)) %>%
  dplyr::select(-Geography_census) %>%
  # merge in district and population data
  dplyr::full_join(output, by = "Geography") %>%
  dplyr::full_join(pop_tracts_total, by = "Geography")

# assess missingness
missing_data2 <- voting_by_district %>%
  dplyr::mutate(
    voting_data_missing_flag = ifelse(is.na(total_votes), 1, 0),
    census_data_missing_flag = ifelse(is.na(Population), 1, 0),
    district_data_missing_flag = ifelse(is.na(district), 1, 0),
    fips_county = stringr::str_sub(Geography, 3, 5),
    vap_over_pop = VAP_MOD / Population
  ) %>%
  dplyr::filter(voting_data_missing_flag == 1 |
                  census_data_missing_flag == 1 |
                  district_data_missing_flag == 1)

missing_census <- missing_data2 %>%
  dplyr::filter(census_data_missing_flag == 1) %>%
  dplyr::pull(Geography) %>%
  sort()

missing_voting <- missing_data2 %>%
  dplyr::filter(voting_data_missing_flag == 1) %>%
  dplyr::pull(Geography) %>%
  sort()




voting_by_district <- voting_by_district %>%
  dplyr::group_by(district) %>%
  dplyr::summarise(
    Population = sum(Population, na.rm = TRUE),
    VAP_MOD = sum(VAP_MOD, na.rm = TRUE),
    total_votes = sum(total_votes, na.rm = TRUE),
    votes_2party = sum(votes_2party, na.rm = TRUE),
    votes_2party_perc = votes_2party / total_votes,
    dem_perc_2party = sum(G20PREDBID, na.rm = TRUE) / votes_2party,
    rep_perc_2party = sum(G20PRERTRU, na.rm = TRUE) / votes_2party
  ) %>%
  dplyr::arrange(as.numeric(district)) %>%
  dplyr::mutate(
    winner = ifelse(dem_perc_2party > .50, "Democrat", "Republican")
  )


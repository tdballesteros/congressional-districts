
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
voting_district_data <- read.csv("Data/poulation_data_2010_by_vtd.csv",
                                 skip = 1,
                                 colClasses = "character")

## Voting Data
# Source: Redistricting Data Hub (RDH)
# Source Website: https://redistrictingdatahub.org/state/ohio/
voting_data <- read.csv("Data/ohio_2020_election_data_by_block.csv",
                        colClasses = "character")

## Output Data
# The output CSV file from the raceData script
output <- read.csv("District Outputs Updated/output_vtd01.csv",
                   colClass = "character")


### format data ----------------------------------------------------------------------
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


voting_data_vtd <- voting_data %>%
  dplyr::mutate(
    G20PREDBID = as.numeric(G20PREDBID),
    G20PREGHAW = as.numeric(G20PREGHAW),
    G20PRELJOR = as.numeric(G20PRELJOR),
    G20PRERTRU = as.numeric(G20PRERTRU),
    VAP_MOD = as.numeric(VAP_MOD),
    total_votes = G20PREDBID + G20PREGHAW + G20PRELJOR + G20PRERTRU
    ) %>%
  dplyr::group_by(COUNTYFP20, PRECINCTID) %>%
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
    Geography = paste0("39", COUNTYFP20, PRECINCTID),
    Geography = gsub(pattern = "-", replacement = "", x = Geography)
  )

### district partisanship ----------------------------------------------------------------------

voting_by_vtd <- output %>%
  dplyr::full_join(voting_data_vtd, by = "Geography") %>%
  dplyr::mutate(
    voting_data_missing_flag = is.na(VAP_MOD)
  ) %>%
  dplyr::group_by(district,voting_data_missing_flag) %>%
  dplyr::summarise(
    VAP_MOD = sum(VAP_MOD, na.rm = TRUE),
    G20PREDBID = sum(G20PREDBID, na.rm = TRUE),
    G20PREGHAW = sum(G20PREGHAW, na.rm = TRUE),
    G20PRELJOR = sum(G20PRELJOR, na.rm = TRUE),
    G20PRERTRU = sum(G20PRERTRU, na.rm = TRUE),
    total_votes = sum(total_votes, na.rm = TRUE),
    votes_2party = sum(votes_2party, na.rm = TRUE),
    votes_2party_perc = votes_2party / total_votes,
    dem_perc_2party = sum(G20PREDBID, na.rm = TRUE) / votes_2party,
    rep_perc_2party = sum(G20PRERTRU, na.rm = TRUE) / votes_2party,
    winner = ifelse(dem_perc_2party > rep_perc_2party, "Democrat", "Republican")
  ) %>%
  dplyr::arrange(as.numeric(district)) %>%
  dplyr::filter(voting_data_missing_flag == FALSE)

missing_vtd_votes <- output %>%
  dplyr::full_join(voting_data_vtd, by = "Geography") %>%
  dplyr::mutate(
    voting_data_missing_flag = is.na(VAP_MOD),
    vap_over_pop = as.numeric(VAP_MOD) / as.numeric(Population)
  )  %>%
  dplyr::select(Geography, district, Population, VAP_MOD, voting_data_missing_flag)

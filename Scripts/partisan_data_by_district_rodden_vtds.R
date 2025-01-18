
# This script takes an output from the randomDistance file, adds voting data, and
# calculates district-wide voting partisanship.


### load libraries ----------------------------------------------------------------------

library(readxl)
library(sf)
library(ggplot2)
library(mapview)
library(tibble)
library(tidyverse)

### 'not in' function ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------

# vtd list with populations
vtd_data <- read.csv("Data/oh_vote_merged_final.csv")
oh2010 <- read.csv("Data/population_data_2010_by_vtd.csv",
                   skip = 1)

## Output Data
# The output CSV file from the random_distance_vtds script
output <- read.csv("District Outputs Rodden/output_vtd01.csv",
                   colClass = "character")

### format data ----------------------------------------------------------------------

pop_vtds <- oh2010 %>%
  dplyr::mutate(GEO_ID = stringr::str_sub(Geography, 10)) %>%
  dplyr::select(Geography,Total,-Geography)

voting_vtd <- vtd_data %>%
  dplyr::select(GEOID10,reg_08,pres_dvote_08,pres_rvote_08,pres_tvote_08) %>%
  dplyr::rename(
    registered_voters = reg_08,
    votes_dem = pres_dvote_08,
    votes_rep = pres_rvote_08,
    votes_total = pres_tvote_08
    ) %>%
  dplyr::mutate(
    votes_3rdparty = votes_total - votes_dem - votes_rep,
    votes_2party = votes_dem + votes_rep
    )

### district partisanship ----------------------------------------------------------------------

voting_by_vtd <- output %>%
  dplyr::full_join(voting_vtd, dplyr::join_by("GEO_ID" == "GEOID10")) %>%
  dplyr::mutate(
    voting_data_missing_flag = is.na(votes_total)
  ) %>%
  dplyr::group_by(district,voting_data_missing_flag) %>%
  dplyr::summarise(
    CIT_EST = sum(as.numeric(CIT_EST), na.rm = TRUE),
    registered_voters = sum(registered_voters, na.rm = TRUE),
    votes_dem = sum(votes_dem, na.rm = TRUE),
    votes_rep = sum(votes_rep, na.rm = TRUE),
    votes_3rdparty = sum(votes_3rdparty, na.rm = TRUE),
    votes_total = sum(votes_total, na.rm = TRUE),
    votes_2party = sum(votes_2party, na.rm = TRUE),
    dem_perc_2party = votes_dem / votes_2party,
    rep_perc_2party = votes_rep / votes_2party,
    winner = ifelse(dem_perc_2party > rep_perc_2party, "Democrat", "Republican")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(as.numeric(district))




# This script takes an output from the randomDistance file, adds voting data, and
# calculates district-wide voting partisanship.

### load libraries ----------------------------------------------------------------------

library(readxl)
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
    District = factor(district,
                      levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15",
                                 "16", "Ohio Total")),
    `Voting Data Missing Flag` = is.na(votes_total)
  ) %>%
  dplyr::group_by(District,`Voting Data Missing Flag`) %>%
  dplyr::summarise(
    Population = sum(as.numeric(CIT_EST), na.rm = TRUE),
    `Registered Voters` = sum(registered_voters, na.rm = TRUE),
    `Votes Democratic` = sum(votes_dem, na.rm = TRUE),
    `Votes Republican` = sum(votes_rep, na.rm = TRUE),
    `Votes Third Party` = sum(votes_3rdparty, na.rm = TRUE),
    `Votes Total` = sum(votes_total, na.rm = TRUE),
    `Votes Two Main Parties` = sum(votes_2party, na.rm = TRUE),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(District) %>%
  tibble::add_row(
    District = "Ohio Total",
    `Voting Data Missing Flag` = FALSE,
    Population = sum(as.numeric(output$CIT_EST), na.rm = TRUE),
    `Registered Voters` = sum(voting_vtd$registered_voters, na.rm = TRUE),
    `Votes Democratic` = sum(voting_vtd$votes_dem, na.rm = TRUE),
    `Votes Republican` = sum(voting_vtd$votes_rep, na.rm = TRUE),
    `Votes Third Party` = sum(voting_vtd$votes_3rdparty, na.rm = TRUE),
    `Votes Total` = sum(voting_vtd$votes_total, na.rm = TRUE),
    `Votes Two Main Parties` = sum(voting_vtd$votes_2party, na.rm = TRUE)
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    `Democrat Percent Two Main Parties` = `Votes Democratic` / `Votes Two Main Parties`,
    `Republican Percent Two Main Parties` = `Votes Republican` / `Votes Two Main Parties`,
    Winner = ifelse(`Democrat Percent Two Main Parties` > `Republican Percent Two Main Parties`, "Democrat", "Republican"),
    Turnout = 100 * `Votes Total` / `Registered Voters`,
    # add Competitiveness Score, where the absolute value of Democrat Percent Two
    # Main Parties - Republican Percent Two Main Parties
    # is classified based on:
    # [0, 3) - 4 (tossup)
    # [3, 10) - 3 (competitive)
    # [10, 20) - 2 (leans)
    # > 20 - 1 (solid)
    `Vote Difference` = abs(`Democrat Percent Two Main Parties` - `Republican Percent Two Main Parties`),
    # Partisan Vote Difference accounts for Democrat/Republican leans (positive is more Democratic, negative
    # is more Republican)
    `Partisan Vote Difference` = `Democrat Percent Two Main Parties` - `Republican Percent Two Main Parties`,
    `Competitiveness Score` = dplyr::case_when(
      `Vote Difference` >= 0.00 &
        `Vote Difference` < 0.03 ~ 1,
      `Vote Difference` >= 0.03 &
        `Vote Difference` < 0.10 ~ 2,
      `Vote Difference` >= 0.10 &
        `Vote Difference` < 0.20 ~ 3,
      `Vote Difference` >= 0.20 ~ 4,
      .default = NA
      ),
    `Partisan Competitiveness Score` = dplyr::case_when(
      `Voting Data Missing Flag` == FALSE & `Partisan Vote Difference` <= -0.20 ~ "Solid Republican",
      `Voting Data Missing Flag` == FALSE & `Partisan Vote Difference` >= 0.20 ~ "Solid Democratic",
      `Voting Data Missing Flag` == FALSE & `Partisan Vote Difference` <= -0.10 ~ "Likely Republican",
      `Voting Data Missing Flag` == FALSE & `Partisan Vote Difference` >= 0.10 ~ "Likely Democratic",
      `Voting Data Missing Flag` == FALSE & `Partisan Vote Difference` <= -0.03 ~ "Leans Republican",
      `Voting Data Missing Flag` == FALSE & `Partisan Vote Difference` >= 0.03 ~ "Leans Democratic",
      `Voting Data Missing Flag` == FALSE ~ "Tossup",
      .default = NA
    ))
    
write.csv(voting_by_vtd, "Districts by Partisanship Rodden/partisanship_vtd01.csv", row.names = FALSE)


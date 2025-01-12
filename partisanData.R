
# This script takes an output from the randomDistance file, adds 2008 voting data, and
# calculates district-wide voting partisanship.


### load libraries ----------------------------------------------------------------------

library(readxl)
library(openxlsx)
library(tidyverse)


### 'not in' function ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

# voting data 2008
voting_data <- readxl::read_xls("Data/2008precincts.xls",
                                skip = 1)

# voting data 2018
voting_data_2018 <- readxl::read_xlsx("Data/oh_2018_gen_2020_blocks.xlsx")

# tract list with populations
tract_data <- read.csv("Data/BlockGr_Ohio.csv")

# output data
output <- read.csv("output1.csv")


### compare voting data ----------------------------------------------------------------------

## compare census tracts within each dataset
tracts2008 <- output %>%
  dplyr::pull(GEO_ID) %>%
  unique()

tracts2018 <- voting_data_2018 %>%
  dplyr::mutate(GEOID20 = str_sub(GEOID20, 1, 11)) %>%
  dplyr::pull(GEOID20) %>%
  unique()

tracts_absent_2008 <- tracts2018[tracts2018 %!in% tracts2008]
tracts_absent_2018 <- tracts2008[tracts2008 %!in% tracts2018]


### format voting_data_2018 ----------------------------------------------------------------------

tracts2018 <- voting_data_2018 %>%
  # pull only geographic information and population estimates
  dplyr::select(GEOID20,COUNTYFP,PRECINCTID,VAP_MOD) %>%
  # filter out entries with 0 population
  dplyr::filter(VAP_MOD > 0) %>%
  dplyr::group_by(PRECINCTID) %>%
  dplyr::mutate(
    precinct_pop = sum(VAP_MOD, na.rm = TRUE),
    perc_of_precinct_pop = VAP_MOD / precinct_pop,
    precinct_code = str_sub(PRECINCTID, -3, -1)
    ) %>%
  dplyr::ungroup()


### format voting_data (2008) ----------------------------------------------------------------------

tracts2008 <- voting_data %>%
  # remove total row
  dplyr::filter(!is.na(`COUNTY NUMBER`)) %>%
  # select precinct information and 2008 presidential election results
  dplyr::select(1:14, - `COUNTY NAME`, -`PRECINCT NAME`) %>%
  # rename candidate variables and county number
  dplyr::rename(
    p01_baldwin_cp = `Baldwin, Chuck (CP)`,
    p02_barr_l = `Barr, Bob (L)`,
    p03_duncan_i = `Duncan, Richard (I)`,
    p04_mccain_r = `McCain, John (R)`,
    p05_mckinney_gr = `McKinney, Cynthia (GR)`,
    p06_moore_so = `Moore, Brian (SO)`,
    p07_nader_i = `Nader, Ralph (I)`,
    p08_obama_d = `Obama, Barack (D)`,
    COUNTYFP = `COUNTY NUMBER`
  ) %>%
  # convert presidential votes to percents of the two-party vote
  dplyr::mutate(
    COUNTYFP = stringr::str_pad(COUNTYFP, 3, "0", side = "left"),
    president_sum = p01_baldwin_cp + p02_barr_l + p03_duncan_i + p04_mccain_r +
      p05_mckinney_gr + p06_moore_so + p07_nader_i + p08_obama_d,
    total_diff = `BALLOTS CAST TOTAL` - president_sum,
    # number of ballots cast for the two main party candidates
    ballots_2party = p04_mccain_r + p08_obama_d,
    # percent of votes for the two main party candidates relative to all votes cast
    ballots_2party_perc = ballots_2party / `BALLOTS CAST TOTAL`,
    # percent of votes for each of the two main party candidates relative to
    # the total number of ballots cast for the two main party candidates
    dem_2party_perc = p08_obama_d / ballots_2party,
    rep_2party_perc = p04_mccain_r / ballots_2party,
    precinct_code = str_sub(`STATE PRC CODE`, -3, -1)
  ) %>%
  dplyr::select(COUNTYFP,`STATE PRC CODE`,precinct_code,`REGISTERED VOTERS TOTAL`,`BALLOTS CAST TOTAL`,
                president_sum,ballots_2party,ballots_2party_perc,dem_2party_perc,
                rep_2party_perc)


### merge voting data ----------------------------------------------------------------------

votingdata <- dplyr::full_join(tracts2008,
                               tracts2018,
                               by = c("COUNTYFP", "precinct_code"))

sum(votingdata$`REGISTERED VOTERS TOTAL`, na.rm = TRUE)
sum(votingdata$VAP_MOD, na.rm = TRUE)


missingness_2018data <- votingdata %>%
  dplyr::filter(is.na(VAP_MOD))

sum(missingness_2018data$`REGISTERED VOTERS TOTAL`, na.rm = TRUE)
# 15.79% of 2008's registered voters are not assigned in 2018

missingness_2008data <- votingdata %>%
  dplyr::filter(is.na(`BALLOTS CAST TOTAL`))

sum(missingness_2008data$VAP_MOD, na.rm = TRUE)
# 81.95% of 2018's estimated population are not assigned in 2008

votingdata <- votingdata %>%
  dplyr::mutate(
    tract_precinct_vote_estimate = ballots_2party * perc_of_precinct_pop,
    vote_estimate_d = tract_precinct_vote_estimate * dem_2party_perc,
    vote_estimate_r = tract_precinct_vote_estimate * rep_2party_perc,
    )


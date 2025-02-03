
# This script compiles Random District Outputs - Tracts 2010 Compactness data

# 2020 actual results
# 12 seats R, 4 seats D
# Popular vote (house races): 56.46% R vs. 42.55% D (+0.99% Libertarian / write-in)
### Two-Party vote split: 57.02% R vs. 42.98% D
# Popular vote (presidential race): 53.27% R vs. 45.24% D

# Expected results -> approximately 9 seats R, 7 seats D


### load libraries ----------------------------------------------------------------------
library(tibble)
library(beeswarm)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

# file path to the folder containing the data
data_folder_path <- "Tracts 2010 (alg2)/Export Data/Districts by Partisanship"

# list all the files within the folder
data_folder_list <- list.files(data_folder_path, full.names = TRUE)

# load all files into a single object
partisanship_data <- lapply(data_folder_list, read.csv)

# actual 2020 election results
results <- readxl::read_xlsx("Data/ohio_2020_congressional_election_results_by_district.xlsx")

# actual 2020 state-wide presidential election results
presidential_results <- data.frame(
  Party = c("Republican", "Democrat", "Libertarian", "Green", "Write-Ins"),
  Votes = c(3154834, 2679165, 67569, 18812, 1822)
  )


### format data ----------------------------------------------------------------------

# calculate expected number of seats for each party based on proportional representation of their
# congressional vote
expected_pr_results <- results %>%
  dplyr::group_by() %>%
  dplyr::summarise(
    `Republican` = sum(`Republican Votes`, na.rm = TRUE),
    `Democrat` = sum(`Democrat Votes`, na.rm = TRUE),
    `Other` = sum(`Other Votes`, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(1:3, names_to = "Party", values_to = "Votes") %>%
  dplyr::mutate(
    `Vote Percentage` = Votes / sum(Votes, na.rm = TRUE),
    `Seat Allocation` = 16 * `Vote Percentage`,
    `Seat Allocation (rounded)` = round(`Seat Allocation`)
  )

# calculate expected number of seats for each party based on state-wide presidential election results
expected_presidential_results <- presidential_results %>%
  dplyr::mutate(
    `Vote Percentage` = Votes / sum(Votes, na.rm = TRUE),
    `Seat Allocation` = 16 * `Vote Percentage`,
    `Seat Allocation (rounded)` = round(`Seat Allocation`)
  )


for(l in 1:length(partisanship_data)){
  names(partisanship_data[[l]]) <- c("District", "Population", "VAP_MOD", "Votes_Democratic","Votes_Republican",
                                     "Votes_Green","Votes_Libertarian","Votes_Total","Votes_2Party",
                                     "Vote_Perc_Democratic_2Party","Vote_Perc_Republican_2Party","Vote_Perc_2Party",
                                     "Turnout_VS_Population","Turnout_VS_VAP","Victory_Margin_2Party","Winner")
}

### statistical tests ----------------------------------------------------------------------
winners_by_run <- data.frame()

for(a in 1:length(partisanship_data)){
  
  tmp <- partisanship_data[[a]] %>%
    dplyr::group_by(Winner) %>%
    dplyr::tally(name = "Won") %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "Winner", values_from = "Won") %>%
    dplyr::mutate(Run = a, .before = 1)
  
  winners_by_run <- rbind(winners_by_run, tmp)
  
}

winners_dem <- winners_by_run %>%
  tidyr::pivot_longer(2:3, names_to = "Winner", values_to = "Value") %>%
  dplyr::filter(Winner == "Democrat")

winners_rep <- winners_by_run %>%
  tidyr::pivot_longer(2:3, names_to = "Winner", values_to = "Value") %>%
  dplyr::filter(Winner == "Republican")

# comparing against PR 2020 congressional results
t.test(x = winners_dem$Value, alternative = "two.sided",
       mu = expected_pr_results$`Seat Allocation`[expected_pr_results$Party=="Democrat"],
       conf.level = 0.95)

t.test(x = winners_rep$Value, alternative = "two.sided",
       mu = expected_pr_results$`Seat Allocation`[expected_pr_results$Party=="Republican"],
       conf.level = 0.95)

# comparing against actual 2020 congressional results
t.test(x = winners_dem$Value, alternative = "two.sided",
       mu = 4,
       conf.level = 0.95)

t.test(x = winners_rep$Value, alternative = "two.sided",
       mu = 12,
       conf.level = 0.95)


hist <- ggplot2::ggplot(data = winners_dem) +
  ggplot2::scale_color_distiller(palette = "Blues", aesthetics = c("color", "fill")) +
  ggplot2::geom_histogram(aes(x = Value, fill = 1), binwidth = .5) +
  ggplot2::xlab("Number of Districts Won by a Democrat") +
  ggplot2::ylab("Frequency")
hist



### win margin by run ----------------------------------------------------------------------

win_margin_absolute <- data.frame()
win_margin_party <- data.frame()

for(a in 1:length(partisanship_data)){
  
  tmp <- partisanship_data[[a]]
  
  win_margin_absolute <- c(win_margin_absolute,mean(abs(tmp$`X..Margin..2P.`)))
  win_margin_party <- c(win_margin_party,mean(tmp$`X..Margin..2P.`))
  
}

win_margin_absolute <- unlist(win_margin_absolute)
win_margin_party <- unlist(win_margin_party)


### plots ----------------------------------------------------------------------

hist_dem <- ggplot2::ggplot(data = winners_by_run) +
  ggplot2::scale_color_distiller(palette = "Blues", aesthetics = c("color", "fill")) +
  ggplot2::geom_histogram(aes(x = Democrat, fill = 1), binwidth = .5)
  
hist_dem

hist <- ggplot2::ggplot(data = winners_long) +
  ggplot2::scale_color_brewer(palette = "Blues", aesthetics = c("color", "fill")) +
  ggplot2::geom_histogram(aes(x = Value, fill = Winner))

hist

# beeswarm(winners_by_run$Democrat)


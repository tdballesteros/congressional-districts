
# This script compiles Random District Outputs - Tracts 2010 Compactness data

# 2020 actual results
# 12 seats R, 4 seats D
# Popular vote (house races): 56.46% R vs. 42.55% D
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
data_folder_path <- "Tracts 2010 (alg1)/Export Data/Districts by Partisanship Tracts 2010"

# list all the files within the folder
data_folder_list <- list.files(data_folder_path, full.names = TRUE)

# load all files into a single object
compactness_data <- lapply(data_folder_list, read.csv)

### calculate data ----------------------------------------------------------------------

### winners by run ----------------------------------------------------------------------

winners_by_run <- data.frame()

for(a in 1:length(compactness_data)){
  
  tmp <- compactness_data[[a]] %>%
    dplyr::group_by(Winner) %>%
    dplyr::tally(name = "Won") %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "Winner", values_from = "Won") %>%
    dplyr::mutate(Run = a, .before = 1)
  
  winners_by_run <- rbind(winners_by_run, tmp)
  
}

winners_long <- winners_by_run %>%
  tidyr::pivot_longer(2:3, names_to = "Winner", values_to = "Value")


### win margin by run ----------------------------------------------------------------------

win_margin_absolute <- data.frame()
win_margin_party <- data.frame()

for(a in 1:length(compactness_data)){
  
  tmp <- compactness_data[[a]]
  
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


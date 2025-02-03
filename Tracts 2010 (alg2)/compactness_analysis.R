
# This script compiles Random District Outputs - Tracts 2010 Compactness data


### load libraries ----------------------------------------------------------------------
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

# file path to the folder containing the data
data_folder_path <- "Tracts 2010 (alg2)/Export Data/Districts by Compactness"

# list all the files within the folder
data_folder_list <- list.files(data_folder_path, full.names = TRUE)

# load all files into a single object
compactness_data <- lapply(data_folder_list, read.csv)

# load actual congrssional district compactness data
compactness_data_actual <- read.csv("Tracts 2010 (alg2)/Export Data/compactness_official_districts.csv")

### compare averages ----------------------------------------------------------------------
average_polsby_popper <- c()
average_schwartzberg <- c()
average_reock <- c()

for(l in 1:length(compactness_data)){
  
  tmp <- compactness_data[[l]]
  
  average_polsby_popper <- c(average_polsby_popper,
                             mean(tmp$Compactness.Polsby.Popper, na.rm = TRUE))
  average_schwartzberg <- c(average_polsby_popper,
                            mean(tmp$Compactness.Schwartzberg, na.rm = TRUE))
  average_reock <- c(average_polsby_popper,
                     mean(tmp$Compactness.Reock, na.rm = TRUE))
  
}

# actual scores by congressional session
average_compactness_scores_actual <- compactness_data_actual %>%
  dplyr::group_by(Congressional.Session) %>%
  dplyr::summarise(
    Compactness.Polsby.Popper = mean(Compactness.Polsby.Popper, na.rm = TRUE),
    Compactness.Schwartzberg = mean(Compactness.Schwartzberg, na.rm = TRUE),
    Compactness.Reock = mean(Compactness.Reock, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()






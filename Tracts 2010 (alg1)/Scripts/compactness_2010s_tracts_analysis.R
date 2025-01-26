
# This script compiles Random District Outputs - Tracts 2010 Compactness data

### load libraries ----------------------------------------------------------------------
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------

# file path to the folder containing the data
data_folder_path <- "Tracts 2010 (alg1)/Export Data/Districts by Compactness Tracts 2010"

# list all the files within the folder
data_folder_list <- list.files(data_folder_path, full.names = TRUE)

# load all files into a single object
compactness_data <- lapply(data_folder_list, read.csv)





# file path to the folder containing the data
data_folder_path <- "Tracts 2010 (alg2)/99_Export Data/District Outputs Tracts 2010 v2"

# list all the files within the folder
data_folder_list <- list.files(data_folder_path, full.names = TRUE) %>%
  sort()

# load all files into a single object
data <- lapply(data_folder_list, read.csv)

missing <- list()

for(i in 1:length(data)){
  
  tmp <- data[[i]]
  
  missing[[i]] <- sum(is.na(tmp$district))
  
}

missing <- unlist(missing)

which(missing > 6)

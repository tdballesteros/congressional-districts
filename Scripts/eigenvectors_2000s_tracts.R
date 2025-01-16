
# This script creates Ohio's 16 congressional districts by calculating eigenvectors using
# Ohio's 2000s Census tract and population data.

### load libraries ----------------------------------------------------------------------

library(readxl)
library(sf)
library(tibble)
library(tidyverse)

### 'not in' function ----------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------

# adjacency matrix
adj <- read.csv("Data/adjacency_list_tracts_2000.csv",
                colClasses = "character")

# tract list with populations
tract_data <- read.csv("Data/population_data_2000_by_tract.csv")

# Ohio shapefile
area <- sf::read_sf(
  dsn = "Data/gz_2010_39_140_00_500k/gz_2010_39_140_00_500k.shp") %>%
  dplyr::rename_with(tolower, -starts_with("GEO_ID"))

### format data ----------------------------------------------------------------------

pop <- tract_data %>%
  dplyr::mutate(geoid = as.character(geoid)) %>%
  dplyr::filter(lntitle == "Total") %>%
  
  # extract FIPS data
  dplyr::mutate(
    stateid = str_sub(geoid, 8, 9),
    countyid = str_sub(geoid, 10, 12),
    tract = str_sub(geoid, 13, 18),
    block = str_sub(geoid, 19, 19),
    GEO_ID = str_sub(geoid, -12, -2)
  ) %>%
  
  # Ohio FIPS code is 39
  dplyr::filter(stateid == 39) %>%
  dplyr::group_by(GEO_ID) %>%
  dplyr::summarise(CIT_EST = sum(CIT_EST, na.rm = TRUE)) %>%
  dplyr::ungroup()

adj2 <- adj %>%
  dplyr::mutate(adjacent = 1) %>%
  tidyr::pivot_wider(names_from = "NEIGHBOR_TRACTID", values_from = "adjacent") %>%
  # change first column to row names and remove column
  tibble::column_to_rownames(var = "SOURCE_TRACTID")
  
# replace all NA values with 0
adj2[is.na(adj2)] <- 0

### recalculate adjacency matrix ----------------------------------------------------------------------

# arrange columns in numeric order
adj2 <- adj2[, order(names(adj2))]
# arrange rows in numeric order
adj2 <- adj2[order(attr(adj2,"row.names")),]

# pull out the non-adjacent tracts and recode self-tracts as 1
eigen_matrix_0 <- adj2 %>%
  tibble::rownames_to_column(var = "SOURCE_TRACTID") %>%
  tidyr::pivot_longer(2:2953, names_to = "NEIGHBOR_TRACTID", values_to = "Value") %>%
  dplyr::filter(Value == 0) %>%
  dplyr::mutate(
    Value = dplyr::case_when(
      SOURCE_TRACTID == NEIGHBOR_TRACTID ~ 1,
      .default = 0
    )
  )

# pull out the adjacent tracts to calculate the number of adjacent tracts
eigen_matrix_1 <- adj2 %>%
  tibble::rownames_to_column(var = "SOURCE") %>%
  tidyr::pivot_longer(2:2953, names_to = "NEIGHBOR_TRACTID", values_to = "Value") %>%
  dplyr::filter(Value == 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Value = -1/nrow(adj %>% dplyr::filter(SOURCE_TRACTID == SOURCE))) %>%
  dplyr::rename(SOURCE_TRACTID = SOURCE)

eigen_matrix <- rbind(eigen_matrix_0,
                      eigen_matrix_1) %>%
  tidyr::pivot_wider(names_from = "NEIGHBOR_TRACTID", values_from = "Value")
rownames(eigen_matrix) <- eigen_matrix$SOURCE_TRACTID

# pull list of row names
eigen_matrix_rows <- eigen_matrix$SOURCE_TRACTID

eigen_matrix <- eigen_matrix %>%
  dplyr::select(-SOURCE_TRACTID)

### compute eigenvalues ----------------------------------------------------------------------

# compute eigenvalues based on the modified adjacency matrix
eigens <- eigen(eigen_matrix)
eigentest <- eigen(adj2)

eigenvectors <- eigens$vectors %>%
  as.data.frame() %>%
  dplyr::mutate(across(everything(), as.numeric))
names(eigenvectors) <- names(eigen_matrix)

eigenvectors <- eigenvectors %>%
  cbind(eigen_matrix_rows) %>%
  dplyr::rename(SOURCE_TRACTID = dplyr::last_col()) %>%
  dplyr::relocate("SOURCE_TRACTID", .before = names(eigen_matrix)[1]) %>%
  tidyr::pivot_longer(2:2953, names_to = "NEIGHBOR_TRACTID", values_to = "Value")

eigenvalues <- eigens$values %>%
  as.data.frame() %>%
  cbind(eigen_matrix_rows) %>%
  dplyr::rename(
    Value = 1,
    SOURCE_TRACTID = 2
  ) %>%
  dplyr::mutate(
    `Value` = as.numeric(`Value`),
    SOURCE_TRACTID = paste0("15000US",SOURCE_TRACTID)) %>%
  dplyr::relocate(SOURCE_TRACTID, .before = Value)

# join eigenvalues with population data
eigen_population <- dplyr::right_join(eigenvalues,
                                     tract_data,
                                     dplyr::join_by("SOURCE_TRACTID" == "geoid")) %>%
  dplyr::mutate(
    Value = ifelse(is.na(Value), 0, Value),
    SOURCE_TRACTID = paste0("1400000US",stringr::str_sub(SOURCE_TRACTID, 8, -2))
    ) %>%
  dplyr::arrange(Value) %>%
  # create a cumulative sum population column
  dplyr::mutate(cmsm = cumsum(CIT_EST))

# calculate target population (half of population)
targpop <- sum(eigen_population$CIT_EST, na.rm = TRUE) / 2

# identify maximum tract that has a cmsm population below the target
below_cmsm_max <- max(which(eigen_population$cmsm < targpop))

halfalpha <- eigen_population[c(1:(below_cmsm_max+1)),1] %>%
  na.omit() %>%
  unique()

halfbeta <- eigen_population[c((below_cmsm_max+2):nrow(eigen_population)),1] %>%
  na.omit() %>%
  unique()


### map data ----------------------------------------------------------------------

area %>%
  dplyr::mutate(
    district = dplyr::case_when(
      GEO_ID %in% halfalpha ~ 1,
      GEO_ID %in% halfbeta ~ 2,
      .default = 0
      ),
    color = dplyr::case_when(
      district == 1 ~ "dodgerblue3",
      district == 2 ~ "firebrick2",
      .default = "chartreuse2"
  )) %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = color))


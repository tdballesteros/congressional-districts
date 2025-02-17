
# This script imports the population/race data for each random district run and runs analysis on them.
# It additionally compares the results to the 113th Congress's population/race data. Note the demographics
# do not align between the 2010 Census data and the 113th Congress's ACS data, though are largely similar.

### load libraries ----------------------------------------------------------------------
library(tibble)
library(beeswarm)
library(ggbeeswarm)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

# set which output file to use when pulling information from only one outut file
output_number <- 2

### load data ----------------------------------------------------------------------
## Population Data
# Source: US Census Bureau
population_data <- read.csv(
  "Data/population_data_2010_by_tract.csv",
  skip = 1,
  colClasses = "character"
)

## 113th Congressional Session Population Data
# Source: American Community Survey (ACS)
congress_pop_data_113 <- read.csv(
  "Data/ACSDP1Y2012.DP05-2025-02-12T024931.csv"
)

## 114th Congressional Session Population Data
# Source: American Community Survey (ACS)
congress_pop_data_114 <- read.csv(
  "Data/ACSDP1Y2014.DP05-2025-02-17T171316.csv"
)

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract_v2 <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10) %>%
  dplyr::filter(Geography %!in% c("39095990000", "39043990100", "39093990200",
                                  "39035990000", "39085990000", "39007990000"))

## Output Data
# The output CSV file from the random districts tracts 2010 script

# file path to the folder containing the data
data_folder_path <- "Tracts 2010 (alg2)/99_Export Data/Districts by Race v2"

# list all the files within the folder
data_folder_list <- list.files(data_folder_path, full.names = TRUE) %>%
  sort()

# load all files into a single object
race_data <- lapply(data_folder_list, read.csv)


### format data ----------------------------------------------------------------------

#### format census race data ----------------------------------------------------------------------
population_data <- population_data %>%
  dplyr::filter(
    # Ohio FIPS code is 39
    stringr::str_sub(Geography, 10, 11) == "39",
    # filter out total row for Ohio
    Geography != "0400000US39",
    # filter out empty Lake Erie tracts
    Geography %!in% c("39095990000", "39043990100", "39093990200",
                      "39035990000", "39085990000", "39007990000")
  ) %>%
  tidyr::pivot_longer(3:73, names_to = "Race", values_to = "Population") %>%
  dplyr::select(-X) %>%
  dplyr::filter(Race %!in% c(
    "Total..Population.of.one.race",
    "Total..Two.or.More.Races",
    "Total..Two.or.More.Races..Population.of.six.races",
    "Total..Two.or.More.Races..Population.of.five.races",
    "Total..Two.or.More.Races..Population.of.four.races",
    "Total..Two.or.More.Races..Population.of.three.races",
    "Total..Two.or.More.Races..Population.of.two.races"
  )) %>%
  dplyr::mutate(
    Geography = stringr::str_sub(Geography, 10),
    # recode race variable names and consolidate multirace into one category
    Race = dplyr::case_when(
      Race == "Total" ~ "Population",
      Race == "Total..Population.of.one.race..American.Indian.and.Alaska.Native.alone" ~ "AIAN",
      Race == "Total..Population.of.one.race..Asian.alone" ~ "Asian",
      Race == "Total..Population.of.one.race..Black.or.African.American.alone" ~ "Black",
      Race == "Total..Population.of.one.race..Native.Hawaiian.and.Other.Pacific.Islander.alone" ~ "NHPI",
      Race == "Total..Population.of.one.race..Some.Other.Race.alone" ~ "Other",
      Race == "Total..Population.of.one.race..White.alone" ~ "White",
      .default = "Two or More Races"
    ),
    # several tracts have non-numeric entries in the Population  field; manually correct these
    # and convert all data to numeric values
    Population = dplyr::case_when(
      Geography %in% c("39035118800", "39035141300", "39035187105", "39035187106", "39035195900", "39055310200",
                       "39055310600", "39055310800", "39055310900", "39055311000", "39055311300", "39055311400",
                       "39055311600", "39055311700", "39055311800", "39055312100", "39055312202", "39055312300",
                       "39055312400", "39085206400", "39155930500") &
        Race == "Population" ~ as.numeric(stringr::str_sub(Population, 1, 4)),
      .default = as.numeric(Population)
    )) %>%
  # sum Population over Geography & Race
  dplyr::group_by(Geography, Race) %>%
  dplyr::summarise(
    Population = sum(Population, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = Race, values_from = Population) %>%
  # reorder columns
  dplyr::select(Geography,Population, White, Black, Asian,
                NHPI, AIAN, `Two or More Races`, Other) %>%
  dplyr::mutate(
    `White Percentage` = White / Population,
    `Black Percentage` = Black / Population,
    `Asian Percentage` = Asian / Population,
    `NHPI Percentage` = NHPI / Population,
    `AIAN Percentage` = AIAN / Population,
    `Two or More Races Percentage` = `Two or More Races` / Population,
    `Other Percentage` = Other / Population,
    `Non-White Percentage` = 1 - `White Percentage`
  ) %>%
  # append geographic information
  dplyr::full_join(shape_tract_v2,
                   by = "Geography")

#### format acs race data ----------------------------------------------------------------------
#### format 113th congress data ----------------------------------------------------------------------
total_pop_113th <- congress_pop_data_113[2,] %>%
  tidyr::pivot_longer(2:65, names_to = "District", values_to = "Population") %>%
  dplyr::select(-`Label..Grouping.`) %>%
  dplyr::filter(stringr::str_detect(District, pattern = "Estimate")) %>%
  dplyr::mutate(
    District = stringr::str_sub(District, start = 23, end = 25),
    District = stringr::str_replace_all(District, pattern = "\\.", replacement = ""),
    District = factor(District, levels = c(1:16)),
    Population = stringr::str_replace_all(Population, pattern = "\\,", replacement = ""),
    Population = as.numeric(Population)
    )

race_pop_113th <- congress_pop_data_113[c(70:84),] %>%
  dplyr::mutate(Race = trimws(`Label..Grouping.`, which = "both", whitespace = "[ \\t\\r\\n\\h]"), .before = 1) %>%
  dplyr::select(-`Label..Grouping.`) %>%
  dplyr::filter(Race %in% c("Hispanic or Latino (of any race)", "White alone", "Black or African American alone",
                            "American Indian and Alaska Native alone", "Asian alone",
                            "Native Hawaiian and Other Pacific Islander alone", "Some other race alone",
                            "Two or more races")) %>%
  tidyr::pivot_longer(2:65, names_to = "District", values_to = "Value") %>%
  dplyr::filter(stringr::str_detect(District, pattern = "Estimate")) %>%
  dplyr::mutate(
    District = stringr::str_sub(District, start = 23, end = 25),
    District = stringr::str_replace_all(District, pattern = "\\.", replacement = ""),
    District = factor(District, levels = c(1:16)),
    Value = stringr::str_replace_all(Value, pattern = "\\,", replacement = ""),
    Value = as.numeric(Value),
    Race = dplyr::case_match(
      Race,
      "American Indian and Alaska Native alone" ~ "AIAN",
      "Asian alone" ~ "Asian",
      "Black or African American alone" ~ "Black",
      "Hispanic or Latino (of any race)" ~ "Hispanic",
      "Native Hawaiian and Other Pacific Islander alone" ~ "NHPI",
      "Some other race alone" ~ "Other",
      "Two or more races" ~ "Two or More Races",
      "White alone" ~ "White"
    )
  ) %>%
  tidyr::pivot_wider(names_from = Race, values_from = "Value")

population_113th <- dplyr::full_join(total_pop_113th,
                                     race_pop_113th,
                                     by = "District") %>%
  dplyr::mutate(
    `Hispanic %` = 100 * Hispanic / Population,
    `White %` = 100 * White / Population,
    `Black %` = 100 * Black / Population,
    `AIAN %` = 100 * AIAN / Population,
    `Asian %` = 100 * Asian / Population,
    `NHPI %` = 100 * NHPI / Population,
    `Other %` = 100 * Other / Population,
    `Two or More Races %` = 100 * `Two or More Races` / Population,
    `Diversity Index` = 1 - ((`Hispanic %`/100)^2 + (`White %`/100)^2 + (`Black %`/100)^2 + (`AIAN %`/100)^2 + (`Asian %`/100)^2 +
                               (`NHPI %`/100)^2 + (`Other %`/100)^2 + (`Two or More Races %`/100)^2),
    `Non-White %` = 100 - `White %`,
    `White Minus Non-White %` = `White %` - `Non-White %`,
    # `Race Score`
    `Population Target Ratio` = Population / (sum(total_pop_113th$Population, na.rm = TRUE)/16)
  )

#### format 114th congress data ----------------------------------------------------------------------
total_pop_114th <- congress_pop_data_114[2,] %>%
  tidyr::pivot_longer(2:69, names_to = "District", values_to = "Population") %>%
  dplyr::select(-`Label..Grouping.`) %>%
  dplyr::filter(
    stringr::str_detect(District, pattern = "Estimate"),
    !stringr::str_detect(District, pattern = "Male|Female"),
    District != "Ohio..Estimate"
    ) %>%
  dplyr::mutate(
    District = stringr::str_sub(District, start = 23, end = 25),
    District = stringr::str_replace_all(District, pattern = "\\.", replacement = ""),
    District = factor(District, levels = c(1:16)),
    Population = stringr::str_replace_all(Population, pattern = "\\,", replacement = ""),
    Population = as.numeric(Population)
  )

race_pop_114th <- congress_pop_data_114[c(70:84),] %>%
  dplyr::mutate(Race = trimws(`Label..Grouping.`, which = "both", whitespace = "[ \\t\\r\\n\\h]"), .before = 1) %>%
  dplyr::select(-`Label..Grouping.`) %>%
  dplyr::filter(Race %in% c("Hispanic or Latino (of any race)", "White alone", "Black or African American alone",
                            "American Indian and Alaska Native alone", "Asian alone",
                            "Native Hawaiian and Other Pacific Islander alone", "Some other race alone",
                            "Two or more races")) %>%
  tidyr::pivot_longer(2:69, names_to = "District", values_to = "Value") %>%
  dplyr::filter(
    stringr::str_detect(District, pattern = "Estimate"),
    District != "Ohio..Estimate") %>%
  dplyr::mutate(
    District = stringr::str_sub(District, start = 23, end = 25),
    District = stringr::str_replace_all(District, pattern = "\\.", replacement = ""),
    District = factor(District, levels = c(1:16)),
    Value = stringr::str_replace_all(Value, pattern = "\\,", replacement = ""),
    Value = as.numeric(Value),
    Race = dplyr::case_match(
      Race,
      "American Indian and Alaska Native alone" ~ "AIAN",
      "Asian alone" ~ "Asian",
      "Black or African American alone" ~ "Black",
      "Hispanic or Latino (of any race)" ~ "Hispanic",
      "Native Hawaiian and Other Pacific Islander alone" ~ "NHPI",
      "Some other race alone" ~ "Other",
      "Two or more races" ~ "Two or More Races",
      "White alone" ~ "White"
    )
  ) %>%
  tidyr::pivot_wider(names_from = Race, values_from = "Value")

population_114th <- dplyr::full_join(total_pop_114th,
                                     race_pop_114th,
                                     by = "District") %>%
  dplyr::mutate(
    `Hispanic %` = 100 * Hispanic / Population,
    `White %` = 100 * White / Population,
    `Black %` = 100 * Black / Population,
    `AIAN %` = 100 * AIAN / Population,
    `Asian %` = 100 * Asian / Population,
    `NHPI %` = 100 * NHPI / Population,
    `Other %` = 100 * Other / Population,
    `Two or More Races %` = 100 * `Two or More Races` / Population,
    `Diversity Index` = 1 - ((`Hispanic %`/100)^2 + (`White %`/100)^2 + (`Black %`/100)^2 + (`AIAN %`/100)^2 + (`Asian %`/100)^2 +
                               (`NHPI %`/100)^2 + (`Other %`/100)^2 + (`Two or More Races %`/100)^2),
    `Non-White %` = 100 - `White %`,
    `White Minus Non-White %` = `White %` - `Non-White %`,
    # `Race Score`
    `Population Target Ratio` = Population / (sum(total_pop_114th$Population, na.rm = TRUE)/16)
  )

#### format race output data ----------------------------------------------------------------------
for(l in 1:length(race_data)){
  names(race_data[[l]]) <- c("District", "Population", "White %", "Black %", "Asian %", "NHPI %", "AIAN %",
                             "Two or More Races %", "Other %", "Non-White %", "White Minus Non-White %",
                             "Race Score", "Diversity Index", "Population Target Ratio")
}

#### pull metrics by run ----------------------------------------------------------------------
white_minus_nonwhite_values <- list()
race_score_values <- list()
diversity_index_values <- list()
population_target_ratio_values <- list()

for(l in 1:length(race_data)){
  
  tmp <- race_data[[l]] %>%
    dplyr::filter(District != "Ohio Total")
  
  white_minus_nonwhite_values[[l]] <- tmp$`White Minus Non-White %`
  race_score_values[[l]] <- tmp$`Race Score`
  diversity_index_values[[l]] <- tmp$`Diversity Index`
  population_target_ratio_values[[l]] <- tmp$`Population Target Ratio`
  
}




#### map race data and one output district ----------------------------------------------------------------------

# ## Format Output Data
# output <- race_data[[output_number]]
# 
# # format output data as a shapefile
# output_shapefile <- shape_tract_v2 %>%
#   dplyr::left_join(output, by = "Geography") %>%
#   sf::st_as_sf()
# 
# for(a in c(1:16)){
#   
#   tmp <- output_shapefile %>%
#     dplyr::filter(district == a) %>%
#     sf::st_union() %>%
#     sf::st_as_sf() %>%
#     dplyr::mutate(district = a, .before = 1) %>%
#     dplyr::rename(geometry = 2)
#   
#   assign(paste0("district_shape_",a),tmp)
#   
# }
# 
# output_shapefile <- mget(ls(pattern="district_shape_")) %>%
#   bind_rows() %>%
#   dplyr::mutate(district = factor(district, c(1:16)))
# 
# rm(list=ls(pattern="^district_shape_"))
# 
# # retrieve coordinate reference system
# # st_crs(output_shapefile)
# 
# 
# 
# # create map
# map <- ggplot2::ggplot() +
#   ggplot2::geom_sf(data = population_data,
#                    ggplot2::aes(fill = `Non-White Percentage`), lwd = 0) +
#   ggplot2::scale_fill_distiller(palette = "YlOrRd") +
#   ggplot2::geom_sf(data = output_shapefile, fill = NA, color = "#000000")
# 
# map
# 
# 
# mapview(population_data, zcol = "Non-White Perc")


### compare census and acs 113th congress data ----------------------------------------------------------------------

# compare total Ohio values between the datasets
pop_totals_census <- population_data %>%
  dplyr::group_by() %>%
  dplyr::summarise(across(c(Population, White, Black, Asian, NHPI, AIAN, `Two or More Races`, Other),
                          \(x) sum(x, na.rm = TRUE))) %>%
  dplyr::ungroup() %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(Census = 1) %>%
  tibble::rownames_to_column(var = "Race")

pop_totals_congress_113 <- population_113th %>%
  dplyr::group_by() %>%
  dplyr::summarise(across(c(Population, White, Black, Asian, NHPI, AIAN, `Two or More Races`, Other),
                          \(x) sum(x, na.rm = TRUE))) %>%
  dplyr::ungroup() %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(Congress_113 = 1) %>%
  tibble::rownames_to_column(var = "Race")

pop_totals_congress_114 <- population_114th %>%
  dplyr::group_by() %>%
  dplyr::summarise(across(c(Population, White, Black, Asian, NHPI, AIAN, `Two or More Races`, Other),
                          \(x) sum(x, na.rm = TRUE))) %>%
  dplyr::ungroup() %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(Congress_114 = 1) %>%
  tibble::rownames_to_column(var = "Race")

pop_totals_comparison <- dplyr::full_join(pop_totals_census,
                                          pop_totals_congress_113,
                                          by = "Race") %>%
  dplyr::full_join(pop_totals_congress_114, by = "Race") %>%
  dplyr::mutate(
    difference_113 = Census - Congress_113,
    difference_114 = Census - Congress_114,
    Census_Perc = round(100 * Census / pop_totals_census$Census[pop_totals_census$Race=="Population"], 2),
    Congress_113_Perc = round(100 * Congress_113 / pop_totals_congress_113$Congress[pop_totals_congress_113$Race=="Population"], 2),
    Congress_114_Perc = round(100 * Congress_114 / pop_totals_congress_114$Congress[pop_totals_congress_114$Race=="Population"], 2)
    )


### analyze data ----------------------------------------------------------------------

# White % - Non-White % summary values by map
white_minus_nonwhite_values_mean <- lapply(white_minus_nonwhite_values, mean) %>% unlist()
white_minus_nonwhite_values_median <- lapply(white_minus_nonwhite_values, median) %>% unlist()
white_minus_nonwhite_values_min <- lapply(white_minus_nonwhite_values, min) %>% unlist()
white_minus_nonwhite_values_max <- lapply(white_minus_nonwhite_values, max) %>% unlist()

# Race Score summary values by map
race_score_values_mean <- lapply(race_score_values, mean) %>% unlist()
race_score_values_median <- lapply(race_score_values, median) %>% unlist()
race_score_values_min <- lapply(race_score_values, min) %>% unlist()
race_score_values_max <- lapply(race_score_values, max) %>% unlist()

# Diversity Index summary values by map
diversity_index_values_mean <- lapply(diversity_index_values, mean) %>% unlist()
diversity_index_values_median <- lapply(diversity_index_values, median) %>% unlist()
diversity_index_values_min <- lapply(diversity_index_values, min) %>% unlist()
diversity_index_values_max <- lapply(diversity_index_values, max) %>% unlist()

# Population Target Ratio summary values by map
population_target_ratio_values_mean <- lapply(population_target_ratio_values, mean) %>% unlist()
population_target_ratio_values_median <- lapply(population_target_ratio_values, median) %>% unlist()
population_target_ratio_values_min <- lapply(population_target_ratio_values, min) %>% unlist()
population_target_ratio_values_max <- lapply(population_target_ratio_values, max) %>% unlist()


### diversity index comparison ----------------------------------------------------------------------

# calculate statewide Ohio Diversity Index using ACS data
ohio_statewide <- population_113th %>%
  dplyr::group_by() %>%
  dplyr::summarise(across(c(Hispanic, White, Black, AIAN, Asian, NHPI, Other, `Two or More Races`), sum, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  t() %>%
  as.data.frame() %>%
  rename(Count = 1) %>%
  dplyr::mutate(
    Perc = Count / sum(population_113th$Population, na.rm = TRUE),
    Perc_Squared = Perc^2)

ohio_acs_diversity_index <- 1 - sum(ohio_statewide$Perc_Squared, na.rm = TRUE)
# 0.3334147 (compared to 0.3319737 using Census data)

diversity_index_df <- data.frame(
  run = c(1:length(diversity_index_values_median)),
  Median = diversity_index_values_median,
  Minimum = diversity_index_values_min,
  Maximum = diversity_index_values_max,
  # group color = 1 is for runs, color = 2 is for 113th congress actuals
  Group = "Runs"
  ) %>%
  tibble::add_row(
    run = NA,
    Median = median(population_113th$`Diversity Index`, na.rm = TRUE),
    Minimum = min(population_113th$`Diversity Index`, na.rm = TRUE),
    Maximum = max(population_113th$`Diversity Index`, na.rm = TRUE),
    Group = "Actual"
  ) %>%
  tidyr::pivot_longer(2:4, names_to = "variable", values_to = "value") %>%
  dplyr::mutate(
    variable = factor(
      variable,
      levels = c("Median", "Minimum", "Maximum")
      ),
    Group = factor(
      Group,
      levels = c("Runs", "Actual")
    ))

ggplot2::ggplot(data = diversity_index_df,
                ggplot2::aes(x = variable, y = value, color = Group)) +
  ggbeeswarm::geom_beeswarm() +
  ggplot2::scale_color_manual(values = c("black", "red")) +
  ggplot2::xlab("Summary Value by Run") +
  ggplot2::ylab("Diversity Index Value") +
  ggplot2::labs(title = "Diversity Index Summary Values by Run")
  

### white % - non-white % comparison ----------------------------------------------------------------------

# calculate statewide Ohio Diversity Index using ACS data
ohio_statewide_white_minus_nonwhite <- population_113th %>%
  dplyr::mutate(
    `Non-White` = Population - White
  ) %>%
  dplyr::group_by() %>%
  dplyr::summarise(across(c(Population, White, `Non-White`), sum, na.rm = TRUE)) %>%
  dplyr::ungroup()

ohio_acs_white_minus_nonwhite <- 100 * (ohio_statewide_white_minus_nonwhite$White / ohio_statewide_white_minus_nonwhite$Population) -
  (ohio_statewide_white_minus_nonwhite$`Non-White` / ohio_statewide_white_minus_nonwhite$Population)
# 61.28048 (compared to 65.37830 using Census data)

white_minus_nonwhite_df <- data.frame(
  run = c(1:length(white_minus_nonwhite_values_median)),
  Median = white_minus_nonwhite_values_median,
  Minimum = white_minus_nonwhite_values_min,
  Maximum = white_minus_nonwhite_values_max,
  # group color = 1 is for runs, color = 2 is for 113th congress actuals
  Group = "Runs"
) %>%
  tibble::add_row(
    run = NA,
    Median = median(population_113th$`White Minus Non-White %`, na.rm = TRUE),
    Minimum = min(population_113th$`White Minus Non-White %`, na.rm = TRUE),
    Maximum = max(population_113th$`White Minus Non-White %`, na.rm = TRUE),
    Group = "Actual"
  ) %>%
  tidyr::pivot_longer(2:4, names_to = "variable", values_to = "value") %>%
  dplyr::mutate(
    variable = factor(
      variable,
      levels = c("Median", "Minimum", "Maximum")
    ),
    Group = factor(
      Group,
      levels = c("Runs", "Actual")
    ))

ggplot2::ggplot(data = white_minus_nonwhite_df,
                ggplot2::aes(x = variable, y = value, color = Group)) +
  ggbeeswarm::geom_beeswarm() +
  ggplot2::scale_color_manual(values = c("black", "red")) +
  ggplot2::xlab("Summary Value by Run") +
  ggplot2::ylab("White % - Non-White %") +
  ggplot2::labs(title = "% White Population Minus % Non-White Population Values by Run")


# beeswarm of all districts - white % minus non-white %
white_minus_nonwhite_all_df <- data.frame(Group = "Runs",
                                          value = unlist(white_minus_nonwhite_values)) %>%
  rbind(data.frame(Group = "Actual",
                   value = c(population_113th$`White Minus Non-White %`)))

ggplot2::ggplot(data = white_minus_nonwhite_all_df,
                ggplot2::aes(x = Group, y = value, color = Group)) +
  ggbeeswarm::geom_beeswarm() +
  ggplot2::scale_color_manual(values = c("black", "red")) +
  ggplot2::xlab("Actual vs. Runs") +
  ggplot2::ylab("White % - Non-White %") +
  ggplot2::labs(title = "% White Population Minus % Non-White Population Values")




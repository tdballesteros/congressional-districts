
# This script takes an output from the randomDistance file, adds race/ethnicity data, and
# calculates district-wide racial composition.


### load libraries ----------------------------------------------------------------------
library(readxl)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))


### load data ----------------------------------------------------------------------

# two-digit code for the output file number
output_number <- "65"

# assign folder filepaths
input_folder <- "Tracts 2010 (alg2)/99_Export Data/District Outputs Tracts 2010 v2"
output_folder <- "Tracts 2010 (alg2)/99_Export Data/Districts by Race v2"

## Population Data
# Source: US Census Bureau
population_data <- read.csv(
  "Data/population_data_2010_by_tract.csv",
  skip = 1,
  colClasses = "character"
)

## Population Data - New
# Source: US Census Bureau
population_data <- read.csv(
  "Data/DECENNIALSF12010.P9.csv",
  colClasses = "character"
)

## Ohio County - FIPS Code Crosswalk
county_fips_xwalk <- read.csv(
  "Data/ohio_county_fips_crosswalk.csv"
)

## Output Data
# The output CSV file from the random districts tracts 2010 script
output <- read.csv(paste0(input_folder,"/output",output_number,".csv"),
                   colClass = "character") %>%
  dplyr::select(Geography, district)


### format data ----------------------------------------------------------------------
county_fips_xwalk <- county_fips_xwalk %>%
  dplyr::mutate(
    county = paste0(county," County"),
    fips = stringr::str_pad(fips,
                            width = 3,
                            side = "left",
                            pad = "0")
  )


pop_tracts <- population_data %>%
  dplyr::mutate(`Label..Grouping.` = trimws(`Label..Grouping.`, which = "both", whitespace = "[ \\t\\r\\n\\h]")) %>%
  dplyr::filter(`Label..Grouping.` %in% c(
    "Total:",
    "Hispanic or Latino",
    "White alone",
    "Black or African American alone",
    "American Indian and Alaska Native alone",
    "Asian alone",
    "Native Hawaiian and Other Pacific Islander alone",
    "Some Other Race alone",
    "Two or More Races:"
    )) %>%
  dplyr::mutate(Race = dplyr::case_match(
    `Label..Grouping.`,
    "Total:" ~ "Population",
    "Hispanic or Latino" ~ "Hispanic",
    "White alone" ~ "White",
    "Black or African American alone" ~ "Black",
    "American Indian and Alaska Native alone" ~ "AIAN",
    "Asian alone" ~ "Asian",
    "Native Hawaiian and Other Pacific Islander alone" ~ "NHPI",
    "Some Other Race alone" ~ "Other",
    "Two or More Races:" ~ "Two or More Races"
  ), .before = 1) %>%
  dplyr::select(-`Label..Grouping.`) %>%
  tidyr::pivot_longer(2:2953, names_to = "Tract", values_to = "Population") %>%
  dplyr::mutate(
    # total populations for these tracts have additional alphanumeric characters
    Population = dplyr::case_when(
      Tract %in% c("Census.Tract.1188..Cuyahoga.County..Ohio",
                    "Census.Tract.1413..Cuyahoga.County..Ohio",
                    "Census.Tract.1871.05..Cuyahoga.County..Ohio",
                    "Census.Tract.1871.06..Cuyahoga.County..Ohio",
                    "Census.Tract.1959..Cuyahoga.County..Ohio",
                    "Census.Tract.3102..Geauga.County..Ohio",
                    "Census.Tract.3106..Geauga.County..Ohio",
                    "Census.Tract.3108..Geauga.County..Ohio",
                    "Census.Tract.3109..Geauga.County..Ohio",
                    "Census.Tract.3110..Geauga.County..Ohio",
                    "Census.Tract.3113..Geauga.County..Ohio",
                    "Census.Tract.3114..Geauga.County..Ohio",
                    "Census.Tract.3116..Geauga.County..Ohio",
                    "Census.Tract.3117..Geauga.County..Ohio",
                    "Census.Tract.3118..Geauga.County..Ohio",
                    "Census.Tract.3121..Geauga.County..Ohio",
                    "Census.Tract.3122.02..Geauga.County..Ohio",
                    "Census.Tract.3123..Geauga.County..Ohio",
                    "Census.Tract.3124..Geauga.County..Ohio",
                    "Census.Tract.2064..Lake.County..Ohio",
                    "Census.Tract.9305..Trumbull.County..Ohio"
      ) & Race == "Population" ~ stringr::str_sub(Population, 1, 5),
      .default = Population
    ),
    Population = stringr::str_replace_all(Population, "\\,", ""),
    Population = as.numeric(Population),
    county_name = stringr::str_sub(Tract, 16, -6),
    county_name = stringr::str_replace_all(county_name, c("[0-9]" = "", "\\." = " ")),
    county_name = trimws(county_name, which = "both", whitespace = "[ \\t\\r\\n\\h]"),
    tract = stringr::str_sub(Tract, 14, 20),
    tract = stringr::str_replace_all(tract, "[A-Za-z]", ""),
    # repeated to remove sets of double periods
    tract = stringr::str_replace_all(tract, "\\.$", ""),
    tract = stringr::str_replace_all(tract, "\\.$", ""),
    tract4 = stringr::str_pad(stringr::str_extract(tract, "^([^.]+)"),
                              width = 4,
                              side = "left",
                              pad = "0"),
    tract2 = stringr::str_pad(stringr::str_extract(tract, "(?<=\\.)[^.]+$"),
                              width = 2,
                              side = "left",
                              pad = "0"),
    tract2 = ifelse(is.na(tract2),"00",tract2),
    tract_formatted = paste0(tract4, tract2)
    ) %>%
  dplyr::left_join(county_fips_xwalk, dplyr::join_by("county_name" == "county")) %>%
  dplyr::mutate(
    Geography = paste0("39",fips,tract_formatted)
  ) %>%
  dplyr::select(Geography, Race, Population) %>%
  tidyr::pivot_wider(names_from = "Race", values_from = "Population") %>%
  # reorder columns
  dplyr::select(Geography, Population, White, Black, Hispanic, Asian, NHPI, AIAN, Other, `Two or More Races`)

district_target_population <- sum(pop_tracts$Population, na.rm = TRUE) / 16


### race data ----------------------------------------------------------------------

numbers <- c(1:60) %>% stringr::str_pad(width = 2, side = "left", pad = "0")

for(output_number in numbers){
  
  ## Output Data
  # The output CSV file from the random districts tracts 2010 script
  output <- read.csv(paste0(input_folder,"/output",output_number,".csv"),
                     colClass = "character") %>%
    dplyr::select(Geography, district)

districts_race_data <- dplyr::full_join(output, pop_tracts, by = "Geography") %>%
  dplyr::mutate(District = factor(district,
                                  levels = c(1:16, "Ohio Total"))) %>%
  dplyr::group_by(District) %>%
  dplyr::summarise(
    Population = sum(Population, na.rm = TRUE),
    `White %` = 100 * sum(White, na.rm = TRUE) / Population,
    `Black %` = 100 * sum(Black, na.rm = TRUE) / Population,
    `Hispanic %` = 100 * sum(Hispanic, na.rm = TRUE) / Population,
    `Asian %` = 100 * sum(Asian, na.rm = TRUE) / Population,
    `NHPI %` = 100 * sum(NHPI, na.rm = TRUE) / Population,
    `AIAN %` = 100 * sum(AIAN, na.rm = TRUE) / Population,
    `Other %` = 100 * sum(Other, na.rm = TRUE) / Population,
    `Two or More Races %` = 100 * sum(`Two or More Races`, na.rm = TRUE) / Population,
    `Non-White %` = 100 - `White %`,
  ) %>%
  dplyr::ungroup() %>%
  # add Ohio total row
  tibble::add_row(
    District = "Ohio Total",
    Population = sum(pop_tracts$Population, na.rm = TRUE),
    `White %` = 100 * sum(pop_tracts$White, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Black %` = 100 * sum(pop_tracts$Black, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Hispanic %` = 100 * sum(pop_tracts$Hispanic, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Asian %` = 100 * sum(pop_tracts$Asian, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `NHPI %` = 100 * sum(pop_tracts$NHPI, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `AIAN %` = 100 * sum(pop_tracts$AIAN, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Other %` = 100 * sum(pop_tracts$Other, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Two or More Races %` = 100 * sum(pop_tracts$`Two or More Races`, na.rm = TRUE) / sum(pop_tracts$Population, na.rm = TRUE),
    `Non-White %` = 100 - `White %`
  ) %>%
  dplyr::mutate(
    `White Minus Nonwhite` = `White %` - `Non-White %`,
    `Diversity Index` = 1 - ((`White %` / 100)^2 + (`Black %` / 100)^2 + (`Hispanic %` / 100)^2 + (`Asian %` / 100)^2 +
      (`NHPI %` / 100)^2 +(`AIAN %` / 100)^2 + (`Other %` / 100)^2) + (`Two or More Races %` / 100)^2,
    # for districts without missing data, calculate population relative to
    # target population ratio
    `Population Target Ratio` = dplyr::case_when(
      District %in% c(1:16) ~ 100 * Population / district_target_population,
      .default = NA
  )) %>%
  dplyr::filter(!is.na(District))


### export data ----------------------------------------------------------------------
write.csv(districts_race_data,
          paste0(output_folder,"/race_tracts",output_number,".csv"),
          row.names = FALSE
          )

}

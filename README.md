# congressional-districts

# Overview -----------------------------------------------------------------------------------------------

# This project's goal is to assess whether simple methods of randomly drawing national congressional districts results in "better" results (this of course not being an entirely objective metric). This project seeks to assess whether a randomly-drawn, contiguous congressional district map can draw maps that are more racially balanced, compact, and proportionally represented than the actual seat distribution.

# For an overview of findings, please see the Results.html file.


# Data Sources -------------------------------------------------------------------------------------------

## Shapefiles
# US Census Tracts 2010 Shapefile (tl_2010_39_tract10.shp) - US Census Bureau and TIGER/Line
# US Census Tracts 2020 Shapefile (tl_2020_39_tract.shp) - US Census Bureau and TIGER/Line
# Voting Districts 2010 Shapefile - US Census Bureau and TIGER/Line
# US 108th Congressional Districts (Ohio) Shapefile (tl_2010_39_cd108.shp) - US Census Bureau and TIGER/Line
# US 111th Congressional Districts (Ohio) Shapefile (tl_2010_39_cd111.shp) - US Census Bureau and TIGER/Line
# US 112th Congressional Districts Shapefile (tl_2011_us_cd112.shp) - US Census Bureau and TIGER/Line
# US 113th Congressional Districts Shapefile (tl_2011_us_cd113.shp) - US Census Bureau and TIGER/Line

## Adjacency Matrices
# Voting District Adjacency Data 2010 - derived from US Census Bureau and TIGER/Line shapefiles
# Census Tract Adjacency Data 2000 - Spatial Strucures in the Social Sciences, Brown University
# Census Tract Adjacency Data 2010 (adjacency_list_tracts_2010.csv) - Spatial Strucures in the Social Sciences, Brown University

## Demographic Data
# 2010 Census Data by Tract (population_data_2010_by_tract.csv) - US Census Bureau

## Voting Data
# Ohio 2020 Election Data (ohio_2020_election_data_by_block.csv) - Redistricting Data Hub (RDH) [https://redistrictingdatahub.org/state/ohio/]


# Methodology --------------------------------------------------------------------------------------------

# This data uses census tracts / voting districts to create 16 national congressional districts in Ohio. Districts for this project are required to be contiguous* and of equal population within a reasonable margin. Any racial/ethnic data is only included after, meaning any majority-minority district requirements are ignored for this project.

# Two random tracts / voting districts are selected, the distance from each tract / voting district to each of the two random tracts / voting districts are calculated, and the tracts / voting districts are split in half based on population sizes. Remaining districts are split based on whether proportionally more neighboring districts are in either half of the split. Those remaining districts who do not border either half at this point are assigned based on the proportional number of neighboring districts after all districts with neighbors in either half are assigned.

# * Note: Voting districts are not all contiguous themselves, resulting in possible visible breaks though all districts remain  technically connected.

# congressional-districts


# This project's goal is to assess whether simple methods of randomly drawing national congressional districts results in "better" results (this of course being not entirely objective metric). The project seeks to assess randomly-drawn, contiguous districts' racial composition and voting inclination to predict district partisan competitiveness and likely winners. These results can then be compared to the real life district winnders, partisan competitiveness, and racial composition.

## Data Sources

### Shapefiles
# Voting Districts 2010 Shapefile - US Census and TIGER/Line
# US 108th Congressional Districts (Ohio) Shapefile - US Census Bureau and TIGER/Line
# US 111th Congressional Districts (Ohio) Shapefile - US Census Bureau and TIGER/Line
# US 112th Congressional Districts Shapefile - US Census Bureau and TIGER/Line
# US 113th Congressional Districts Shapefile - US Census Bureau and TIGER/Line

### Adjacency Matrices
# Voting District Adjacency Data 2010 - derived from US Census Bureau and TIGER/Line shapefiles
# Census Tract Adjacency Data 2000 - Spatial Strucures in the Social Sciences, Brown University
# Census Tract Adjacency Data 2010 - Spatial Strucures in the Social Sciences, Brown University

### Demographic Data

### Voting Data

## Methodology
# This data uses census tracts / voting districts to create 16 national congressional districts in Ohio. Districts for this project are required to be contiguous* and of equal population within a reasonable margin. Any racial/ethnic data is only included after, meaning any majority-minority district requirements are ignored for this project.

# Two random tracts / voting districts are selected, the distance from each tract / voting district to each of the two random tracts / voting districts are calculated, and the tracts / voting districts are split in half based on population sizes. Remaining districts are split based on whether proportionally more neighboring districts are in either half of the split. Those remaining districts who do not border either half at this point are assigned based on the proportional number of neighboring districts after all districts with neighbors in either half are assigned.

# * Note: Voting districts are not all contiguous themselves, resulting in possible visible breaks though all districts remain  technically connected.
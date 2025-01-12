VEST 2016 Oregon Election Data Projected to 2020 block

## RDH Date Retrieval
09/13/2021

## RDH Upload Date
09/15/2021

## Sources
The RDH retrieved VEST's data projected to 2020 VTDs from the "census-2020" Github repository from the "alarm-redist" project (https://github.com/alarm-redist/census-2020/tree/main/vest-2020).

## Fields Metadata From VEST

Vote Column Label Format
------------------------
Columns reporting votes follow a standard label pattern. One example is:
G20PRERTRU
The first character is G for a general election, C for recount results, P for a primary, S for a special, and R for a runoff.
Characters 2 and 3 are the year of the election.
Characters 4-6 represent the office type (see list below).
Character 7 represents the party of the candidate.
Characters 8-10 are the first three letters of the candidate's last name.

Office Codes

AGR - Agriculture Commissioner
ATG - Attorney General
AUD - Auditor
COC - Corporation Commissioner
COU - City Council Member
DEL - Delegate to the U.S. House
GOV - Governor
H## - U.S. House, where ## is the district number. AL: at large.
INS - Insurance Commissioner
LAB - Labor Commissioner
LAN - Commissioner of Public Lands
LTG - Lieutenant Governor
PRE - President
PSC - Public Service Commissioner
RRC - Railroad Commissioner
SAC - State Appeals Court (in AL: Civil Appeals)
SCC - State Court of Criminal Appeals
SOS - Secretary of State
SSC - State Supreme Court
SPI - Superintendent of Public Instruction
TRE - Treasurer
USS - U.S. Senate

Party Codes
D and R will always represent Democrat and Republican, respectively.
See the state-specific notes for the remaining codes used in a particular file; note that third-party candidates may appear on the ballot under different party labels in different states.

## Fields

**Fields:**		**Descriptions:**
GEOID			 14-digit block geoid for year 2020
area_land			 area in square meters of Census Block land
area_water			 area in square meters of Census Block covered by water

## Processing Steps
VEST Steps:
1) Match VEST precincts to 2010 Census blocks
2) Add populations to 2010 blocks
3) Estimate election data down to 2010 blocks
4) Crosswalk election data to 2020 blocks by VEST's land use area.
5) Aggregate to 2020 VTDs by BAFs

VEST's code to complete the above steps can be found [here:](https://github.com/alarm-redist/census-2020/blob/main/R/00_build_vest.R)

## Additional Notes
From VEST's description on Github:
"This folder contains intermediate data consisting of VEST election returns projected from 2010-era VTDs to 2020 VTDs (where available) or 2020 Census blocks (where not)."

Please contact info@redistrictingdatahub.org for more information.
    
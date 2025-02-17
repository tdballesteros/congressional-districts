
# This script takes an output from the random_distance file, adds voting data, and
# calculates district-wide voting partisanship.

### load libraries ----------------------------------------------------------------------
library(readxl)
library(tibble)
library(tidyverse)

# 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

### load data ----------------------------------------------------------------------

# two-digit code for the output file number
output_number <- "60"

# assign folder filepaths
input_folder <- "Tracts 2010 (alg2)/99_Export Data/District Outputs Tracts 2010 v2"
output_folder <- "Tracts 2010 (alg2)/99_Export Data/Districts by Partisanship v2"

## Population Data
# Source: US Census Bureau
population_data <- read.csv(
  "Data/population_data_2010_by_tract.csv",
  skip = 1,
  colClasses = "character"
)

## Voting Data
# Source: Redistricting Data Hub (RDH)
# Source Website: https://redistrictingdatahub.org/state/ohio/
voting_data <- read.csv(
  "Data/ohio_2020_election_data_by_block.csv",
  colClasses = "character"
  )

## Output Data
# The output CSV file from the raceData script
output <- read.csv(
  paste0(input_folder,"/output",output_number,".csv"),
  colClass = "character"
  ) %>%
  dplyr::select(Geography, district)

## Map Shapefiles
# Source: US Census Bureau and TIGER/Line
shape_tract <- sf::read_sf(
  dsn = "Data/tl_2010_39_tract10/tl_2010_39_tract10.shp") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename(Geography = geoid10)

# # 2020 map
# shape_tract_2020 <- sf::read_sf(
#   dsn = "Data/tl_2020_39_tract/tl_2020_39_tract.shp") %>%
#   dplyr::rename_with(tolower)
# names(shape_tract_2020)[4] <- "Geography"

### format data ----------------------------------------------------------------------
pop_tracts_total <- population_data %>%
  dplyr::filter(
    # Ohio FIPS code is 39
    stringr::str_sub(Geography, 10, 11) == "39",
    # filter out total row for Ohio
    Geography != "0400000US39"
  ) %>%
  dplyr::select(Geography, Total) %>%
  dplyr::rename(Population = Total) %>%
  # several tracts have non-numeric entries in the Population  field; manually correct these
  # and convert all data to numeric values
  dplyr::mutate(
    Geography = stringr::str_sub(Geography, 10),
    Population = dplyr::case_when(
      Geography %in% c("39035118800", "39035141300", "39035187105", "39035187106", "39035195900", "39055310200",
                       "39055310600", "39055310800", "39055310900", "39055311000", "39055311300", "39055311400",
                       "39055311600", "39055311700", "39055311800", "39055312100", "39055312202", "39055312300",
                       "39055312400", "39085206400", "39155930500")  ~ as.numeric(stringr::str_sub(Population, 1, 4)),
      .default = as.numeric(Population)
    ))


voting_tracts_total <- voting_data %>%
  dplyr::mutate(Geography = paste0("39",COUNTYFP20, TRACTCE20)) %>%
  dplyr::select(Geography, VAP_MOD, G20PREDBID, G20PREGHAW, G20PRELJOR, G20PRERTRU) %>%
  # recode to match tract data
  dplyr::mutate(
    Geography = dplyr::case_when(
      Geography %in% c("39001770301","39001770302") ~ "39001770300",
      Geography %in% c("39003010801","39003010802") ~ "39003010800",
      Geography %in% c("39003011301","39003011302") ~ "39003011300",
      Geography %in% c("39005970101","39005970102") ~ "39005970100",
      Geography %in% c("39007001201","39007001202") ~ "39007001200",
      Geography %in% c("39007001303","39007001304") ~ "39007001302",
      Geography %in% c("39007001401","39007001402") ~ "39007001400",
      Geography %in% c("39013010901","39013010902") ~ "39013010900",
      Geography %in% c("39013012201","39013012202") ~ "39013012200",
      Geography %in% c("39015951301","39015951302") ~ "39015951300",
      Geography %in% c("39017010105","39017010106") ~ "39017010101",
      Geography %in% c("39017010912","39017010913") ~ "39017010908",
      Geography %in% c("39017011005","39017011006") ~ "39017011002",
      Geography %in% c("39023002607","39023002608") ~ "39023002606",
      Geography %in% c("39025040205","39025040206") ~ "39025040204",
      Geography %in% c("39025040703","39025040704") ~ "39025040701",
      Geography %in% c("39025041001","39025041002") ~ "39025041000",
      Geography %in% c("39025041104","39025041105") ~ "39025041103",
      Geography %in% c("39025041201","39025041202") ~ "39025041200",
      Geography %in% c("39025042001","39025042002") ~ "39025042000",
      Geography %in% c("39027964501","39027964502") ~ "39027964500",
      Geography %in% c("39029951401","39029951402") ~ "39029951400",
      Geography == "39035101201" ~ "39035101200",
      Geography == "39035117203" ~ "39035117202",
      Geography == "39035131105" ~ "39035131102",
      Geography %in% c("39035136104","39035136105") ~ "39035136102",
      Geography %in% c("39035172104","39035172105") ~ "39035172103",
      Geography %in% c("39035190505","39035190506") ~ "39035190504",
      Geography %in% c("39037555001","39037555002") ~ "39037555000",
      Geography %in% c("39037560101","39037560102") ~ "39037560100",
      Geography %in% c("39041011431","39041011432") ~ "39041011413",
      Geography %in% c("39043040801","39043040802") ~ "39043040800",
      Geography %in% c("39043041701","39043041702") ~ "39043041700",
      Geography %in% c("39045030601","39045030602") ~ "39045030600",
      Geography %in% c("39045030901","39045030902") ~ "39045030900",
      Geography %in% c("39045031401","39045031402") ~ "39045031400",
      Geography %in% c("39045032501","39045032502") ~ "39045032500",
      Geography %in% c("39045032601","39045032602") ~ "39045032600",
      Geography %in% c("39045032703","39045032704") ~ "39045032702",
      Geography %in% c("39045032901","39045032902") ~ "39045032900",
      Geography %in% c("39045033101","39045033102") ~ "39045033100",
      Geography %in% c("39049001301","39049001302") ~ "39049001300",
      Geography %in% c("39049004301","39049004302") ~ "39049004300",
      Geography %in% c("39049005001","39049005002") ~ "39049005000",
      Geography == "39049006372" ~ "39049006371",
      Geography %in% c("39049006991","39049006992") ~ "39049006990",
      Geography %in% c("39049007552","39049007553") ~ "39049007551",
      Geography %in% c("39049008001","39049008002") ~ "39049008000",
      Geography %in% c("39049008381","39049008382") ~ "39049008370",
      Geography %in% c("39049009801","39049009802") ~ "39049009800",
      Geography %in% c("39049010201","39049010202","39049010203","39049010204") ~ "39049010200",
      Geography %in% c("39049010401","39049010402") ~ "39049010400",
      Geography %in% c("39049010501","39049010502") ~ "39049010500",
      Geography %in% c("39051040701","39051040702") ~ "39051040700",
      Geography %in% c("39053953901","39053953902") ~ "39053953900",
      Geography %in% c("39057200901","39057200902") ~ "39057200900",
      Geography %in% c("39057210101","39057210102") ~ "39057210100",
      Geography %in% c("39057210201","39057210202") ~ "39057210200",
      Geography %in% c("39057210604","39057210605") ~ "39057210601",
      Geography %in% c("39057220101","39057220102") ~ "39057220100",
      Geography %in% c("39057220201","39057220202") ~ "39057220200",
      Geography %in% c("39057240303","39057240304") ~ "39057240301",
      Geography %in% c("39061002901","39061002902") ~ "39061002900",
      Geography %in% c("39061006501","39061006502") ~ "39061006500",
      Geography %in% c("39061020603","39061020604") ~ "39061020602",
      Geography %in% c("39061020763","39061020764") ~ "39061020761",
      Geography %in% c("39061024323","39061024324") ~ "39061024321",
      Geography %in% c("39061024401","39061024402") ~ "39061024400",
      Geography %in% c("39061024903","39061024904") ~ "39061024902",
      Geography %in% c("39061026103","39061026104") ~ "39061026101",
      Geography %in% c("39063000301","39063000302") ~ "39063000300",
      Geography %in% c("39063000901","39063000902") ~ "39063000900",
      Geography %in% c("39065000401","39065000402") ~ "39065000400",
      Geography %in% c("39071955001","39071955002") ~ "39071955000",
      Geography %in% c("39071955101","39071955102") ~ "39071955100",
      Geography %in% c("39075976401","39075976402","39075976403") ~ "39075976400",
      Geography %in% c("39083007301","39083007302") ~ "39083007300",
      Geography %in% c("39083007701","39083007702") ~ "39083007700",
      Geography %in% c("39085201101","39085201102") ~ "39085201100",
      Geography %in% c("39085202901","39085202902") ~ "39085202900",
      Geography %in% c("39087050501","39087050502") ~ "39087050500",
      Geography %in% c("39087051101","39087051102") ~ "39087051100",
      Geography %in% c("39089752801","39089752802") ~ "39089752800",
      Geography %in% c("39089753301","39089753302") ~ "39089753300",
      Geography %in% c("39089754103","39089754104") ~ "39089754101",
      Geography %in% c("39089755301","39089755302") ~ "39089755300",
      Geography %in% c("39089755601","39089755602") ~ "39089755600",
      Geography %in% c("39089755901","39089755902") ~ "39089755900",
      Geography %in% c("39089756203","39089756204") ~ "39089756202",
      Geography %in% c("39089756801","39089756802","39089756803") ~ "39089756800",
      Geography %in% c("39089757401","39089757402") ~ "39089757400",
      Geography %in% c("39089757701","39089757702") ~ "39089757700",
      Geography %in% c("39089758601","39089758602") ~ "39089758600",
      Geography %in% c("39089758901","39089758902") ~ "39089758900",
      Geography %in% c("39089759101","39089759102") ~ "39089759100",
      Geography %in% c("39093013101","39093013102") ~ "39093013100",
      Geography %in% c("39093013201","39093013202") ~ "39093013200",
      Geography %in% c("39093030101","39093030102") ~ "39093030100",
      Geography %in% c("39093050301","39093050302") ~ "39093050300",
      Geography %in% c("39093080701","39093080702") ~ "39093080700",
      Geography %in% c("39093094101","39093094102") ~ "39093094100",
      Geography %in% c("39093097201","39093097202") ~ "39093097200",
      Geography %in% c("39093097401","39093097402") ~ "39093097400",
      Geography %in% c("39095000201","39095000202") ~ "39095000200",
      Geography %in% c("39095000301","39095000302") ~ "39095000300",
      Geography %in% c("39095000601","39095000602") ~ "39095000600",
      Geography %in% c("39095000701","39095000702","39095000703") ~ "39095000700",
      Geography %in% c("39095001001","39095001002") ~ "39095001000",
      Geography %in% c("39095001601","39095001602") ~ "39095001600",
      Geography %in% c("39095003901","39095003902") ~ "39095003900",
      Geography %in% c("39095004401","39095004402") ~ "39095004400",
      Geography %in% c("39095005101","39095005102") ~ "39095005100",
      Geography %in% c("39095005601","39095005602") ~ "39095005600",
      Geography %in% c("39095005704","39095005705") ~ "39095005702",
      Geography %in% c("39095005803","39095005804") ~ "39095005802",
      Geography %in% c("39095006801","39095006802") ~ "39095006800",
      Geography %in% c("39095007003","39095007004") ~ "39095007001",
      Geography %in% c("39095007401","39095007402") ~ "39095007400",
      Geography %in% c("39095007501","39095007502") ~ "39095007500",
      Geography %in% c("39095007801","39095007802") ~ "39095007800",
      Geography %in% c("39095007903","39095007904") ~ "39095007902",
      Geography %in% c("39095008303","39095008304") ~ "39095008301",
      Geography %in% c("39095008401","39095008402") ~ "39095008400",
      Geography %in% c("39095008501","39095008502") ~ "39095008500",
      Geography %in% c("39095008601","39095008602") ~ "39095008600",
      Geography %in% c("39095008701","39095008702","39095008703") ~ "39095008700",
      Geography %in% c("39095008801","39095008802") ~ "39095008800",
      Geography %in% c("39095009001","39095009002","39095009003") ~ "39095009000",
      Geography %in% c("39095009103","39095009104") ~ "39095009102",
      Geography %in% c("39095009901","39095009902") ~ "39095009900",
      Geography %in% c("39099800501","39099800502") ~ "39099800500",
      Geography %in% c("39099811301","39099811302") ~ "39099811300",
      Geography %in% c("39099812101","39099812102") ~ "39099812100",
      Geography %in% c("39099812101","39099812102") ~ "39099812100",
      Geography %in% c("39099812604","39099812605") ~ "39099812603",
      Geography %in% c("39099813501","39099813502") ~ "39099813500",
      Geography %in% c("39099813601","39099813602") ~ "39099813600",
      Geography %in% c("39103408101","39103408102") ~ "39103408100",
      Geography %in% c("39103408303","39103408304","39103408305") ~ "39103408302",
      Geography %in% c("39103415801","39103415802") ~ "39103415800",
      Geography %in% c("39103417001","39103417002") ~ "39103417000",
      Geography %in% c("39109350101","39109350102") ~ "39109350100",
      Geography %in% c("39109365001","39109365002") ~ "39109365000",
      Geography %in% c("39113001801","39113001802") ~ "39113001800",
      Geography %in% c("39113050106","39113050107") ~ "39113050103",
      Geography %in% c("39113125103","39113125104") ~ "39113125101",
      Geography %in% c("39117965201","39117965202") ~ "39117965200",
      Geography %in% c("39117965401","39117965402") ~ "39117965400",
      Geography %in% c("39117965501","39117965502") ~ "39117965500",
      Geography %in% c("39119911201","39119911202") ~ "39119911200",
      Geography %in% c("39119911601","39119911602") ~ "39119911600",
      Geography %in% c("39121968401","39121968402") ~ "39121968400",
      Geography %in% c("39123051201","39123051202","39123051203") ~ "39123051200",
      Geography %in% c("39127965801","39127965802") ~ "39127965800",
      Geography %in% c("39127965901","39127965902") ~ "39127965900",
      Geography %in% c("39127966301","39127966302") ~ "39127966300",
      Geography %in% c("39129021101","39129021102") ~ "39129021100",
      Geography %in% c("39129021201","39129021202") ~ "39129021200",
      Geography %in% c("39129021403","39129021404") ~ "39129021402",
      Geography %in% c("39131952601","39131952602") ~ "39131952600",
      Geography %in% c("39133600303","39133600304") ~ "39133600301",
      Geography %in% c("39133601703","39133601704") ~ "39133601701",
      Geography %in% c("39133602101","39133602102") ~ "39133602100",
      Geography %in% c("39137030301","39137030302") ~ "39137030300",
      Geography %in% c("39141955801","39141955802") ~ "39141955800",
      Geography %in% c("39145002901","39145002902") ~ "39145002900",
      Geography %in% c("39151700701","39151700702") ~ "39151700700",
      Geography %in% c("39151711001","39151711002") ~ "39151711000",
      Geography %in% c("39151711323","39151711324") ~ "39151711311",
      Geography %in% c("39151712113","39151712114") ~ "39151712111",
      Geography %in% c("39153507501","39153507502") ~ "39153507500",
      Geography %in% c("39153531406","39153531407") ~ "39153531401",
      Geography %in% c("39153531501","39153531502") ~ "39153531500",
      Geography %in% c("39157020801","39157020802") ~ "39157020800",
      Geography %in% c("39157021501","39157021502","39157021503") ~ "39157021500",
      Geography %in% c("39159050401","39159050402") ~ "39159050400",
      Geography %in% c("39159050501","39159050502") ~ "39159050500",
      Geography %in% c("39159050701","39159050702") ~ "39159050700",
      Geography %in% c("39165030901","39165030902") ~ "39165030900",
      Geography %in% c("39165031001","39165031002") ~ "39165031000",
      Geography %in% c("39165031601","39165031602","39165031603") ~ "39165031600",
      Geography %in% c("39165032203","39165032204","39165032205","39165032206") ~ "39165032201",
      Geography %in% c("39167020201","39167020202") ~ "39167020200",
      Geography %in% c("39167021201","39167021202") ~ "39167021200",
      Geography %in% c("39173020701","39173020702") ~ "39173020700",
      Geography %in% c("39173020901","39173020902") ~ "39173020900",
      Geography %in% c("39173021601","39173021602") ~ "39173021600",
      Geography %in% c("39017011136","39017011137") ~ "39017011116",
      Geography %in% c("39017011134","39017011135") ~ "39017011117",
      # NOTE: 39017011132 and 39017011121 both contain a small area each that are not in the other tract boundaries
      Geography %in% c("39017011132","39017011133") ~ "39017011121",
      Geography %in% c("39025041505","39025041506") ~ "39025041501",
      Geography %in% c("39025041503","39025041504") ~ "39025041502",
      Geography %in% c("39035175109","39035175110") ~ "39035175103",
      Geography %in% c("39035175107","39035175108") ~ "39035175104",
      Geography %in% c("39035160603","39035160604") ~ "39035160601",
      Geography %in% c("39041011562","39041011563") ~ "39041011550",
      Geography %in% c("39041011564","39041011565") ~ "39041011560",
      Geography %in% c("39041011765","39041011766") ~ "39041011730",
      Geography %in% c("39041011763","39041011764") ~ "39041011750",
      Geography %in% c("39049004001","39049004002") ~ "39049004000",
      Geography %in% c("39049006397","39049006398") ~ "39049006383",
      Geography %in% c("39049006239","39049006240","39049006241") ~ "39049006230",
      Geography %in% c("39049007961","39049007962") ~ "39049007921",
      Geography %in% c("39049006237","39049006238") ~ "39049006220",
      Geography %in% c("39049007963","39049007964") ~ "39049007951",
      Geography %in% c("39049007957","39049007958") ~ "39049007952",
      Geography %in% c("39049007955","39049007956") ~ "39049007933",
      Geography %in% c("39049007965","39049007966") ~ "39049007954",
      Geography %in% c("39049007959","39049007960") ~ "39049007953",
      Geography %in% c("39049008171","39049008172") ~ "39049008142",
      Geography %in% c("39049008165","39049008166") ~ "39049008141",
      Geography %in% c("39049008169","39049008170") ~ "39049008162",
      Geography %in% c("39049008167","39049008168") ~ "39049008161",
      Geography %in% c("39049009756","39049009757") ~ "39049009720",
      Geography %in% c("39049009753","39049009754","39049009755") ~ "39049009740",
      Geography %in% c("39049009401","39049009498") ~ "39049009430",
      Geography %in% c("39049009403","39049009404","39049009405") ~ "39049009450",
      Geography %in% c("39049009251","39049009252") ~ "39049009210",
      Geography %in% c("39049007301","39049007302","39049007397") ~ "39049007395",
      Geography %in% c("39049007303","39049007398") ~ "39049007396",
      Geography %in% c("39049009394","39049009395") ~ "39049009362",
      Geography %in% c("39049009396","39049009397") ~ "39049009371",
      Geography %in% c("39049009391","39049009392") ~ "39049009374",
      Geography %in% c("39049007305","39049007306") ~ "39049007393",
      Geography %in% c("39049007103","39049010900") ~ "39049007132",
      Geography %in% c("39049007211","39049007212") ~ "39049007210",
      Geography %in% c("39049007214","39049007215") ~ "39049007207",
      Geography %in% c("39049007101","39049007102") ~ "39049007194",
      Geography == "39085204303" ~ "39085204301",
      Geography == "39085204304" ~ "39085204302",
      Geography %in% c("39095009203","39095009204") ~ "39095009201",
      Geography %in% c("39095008204","39095008205") ~ "39095008201",
      Geography %in% c("39095008206","39095008207") ~ "39095008202",
      Geography %in% c("39095008208","39095008209","39095008210") ~ "39095008203",
      Geography %in% c("39095009205","39095009206") ~ "39095009202",
      Geography %in% c("39095007304","39095007305") ~ "39095007301",
      Geography %in% c("39095007306","39095007307") ~ "39095007303",
      Geography %in% c("39095007206","39095007207") ~ "39095007203",
      Geography %in% c("39095007208","39095007209") ~ "39095007205",
      Geography %in% c("39095007103","39095007104") ~ "39095007101",
      Geography %in% c("39095008903","39095008904") ~ "39095008901",
      Geography %in% c("39095008905","39095008906") ~ "39095008902",
      Geography %in% c("39165031907","39165031908") ~ "39165031903",
      Geography %in% c("39165031905","39165031906") ~ "39165031902",
      Geography %in% c("39165032008","39165032009") ~ "39165032005",
      Geography %in% c("39165032010","39165032011") ~ "39165032007",
      Geography %in% c("39049006301","39049006302") ~ "39049006371",
      .default = Geography
    )) %>%
  dplyr::group_by(Geography) %>%
  dplyr::summarise(
    VAP_MOD = sum(as.numeric(VAP_MOD), na.rm = TRUE),
    G20PREDBID = sum(as.numeric(G20PREDBID), na.rm = TRUE),
    G20PREGHAW = sum(as.numeric(G20PREGHAW), na.rm = TRUE),
    G20PRELJOR = sum(as.numeric(G20PRELJOR), na.rm = TRUE),
    G20PRERTRU = sum(as.numeric(G20PRERTRU), na.rm = TRUE)
  ) %>%
  dplyr::ungroup()
  
merged_tracts <- list(
  c("39035161700", "39035161800", "39035197300"),
  c("39035106300", "39035106400", "39035197400"),
  c("39035103100", "39035103400", "39035197500"),
  c("39035104300", "39035104200", "39035197800"),
  c("39035103900", "39035104100", "39035197700"),
  c("39035104600", "39035104900", "39035197600"),
  c("39035110501", "39035110801", "39035197900"),
  c("39035160700", "39035160800", "39035161900"),
  c("39035115200", "39035115100", "39035198100"),
  c("39035115300", "39035114900", "39035198000"),
  c("39035154300", "39035154700", "39035198200"),
  c("39035188104", "39035193800", "39035197000"),
  c("39035188105", "39035193900", "39035198300"),
  c("39035194900", "39035194800", "39035197100"),
  c("39035114700", "39035114300", "39035114800"),
  c("39035113801", "39035114100", "39035198400"),
  c("39035113101", "39035196500", "39035197200"),
  c("39035113500", "39035113600", "39035198500"),
  c("39035112400", "39035112800", "39035198700"),
  c("39035112500", "39035112600", "39035198800"),
  c("39035111800", "39035111902", "39035199000"),
  c("39035111500", "39035111600", "39035198900"),
  c("39035116100", "39035116200", "39035199200"),
  c("39035118400", "39035118500", "39035199100"),
  c("39035118700", "39035119100", "39035196800"),
  c("39035119202", "39035119300", "39035198600"),
  c("39035151400", "39035151100", "39035199300"),
  c("39035152603", "39035152604", "39035152605"),
  c("39141955601", "39141955602", "39141955604"),
  c("39043040600", "39043041500", "39043041900"),
  c("39049009333", "39049009331", "39049009393"),
  c("39049007203", "39049007201", "39049007213"),
  c("39061007900", "39061007800", "39061027700"),
  c("39061005400", "39061010800", "39061027600"),
  c("39061004701", "39061004702", "39061004703"),
  c("39061004500", "39061025104", "39061027500"),
  c("39081001700", "39081000800", "39081012400"),
  c("39085206000", "39085205900", "39085206700"),
  c("39093023700", "39093023800", "39093097500"),
  c("39093070800", "39093071000", "39093097600"),
  c("39095002200", "39095002300", "39095010500"),
  c("39095003400", "39095003700", "39095010600"),
  c("39099800400", "39099800300", "39099814200"),
  c("39155920600", "39155920500", "39155934000")
)


full_voting_data <- dplyr::full_join(output, voting_tracts_total, by = "Geography") %>%
  dplyr::full_join(pop_tracts_total, by = "Geography") %>%
  dplyr::mutate(
    merge = dplyr::case_when(
      Geography %in% unlist(merged_tracts[1]) ~ "1",
      Geography %in% unlist(merged_tracts[2]) ~ "2",
      Geography %in% unlist(merged_tracts[3]) ~ "3",
      Geography %in% unlist(merged_tracts[4]) ~ "4",
      Geography %in% unlist(merged_tracts[5]) ~ "5",
      Geography %in% unlist(merged_tracts[6]) ~ "6",
      Geography %in% unlist(merged_tracts[7]) ~ "7",
      Geography %in% unlist(merged_tracts[8]) ~ "8",
      Geography %in% unlist(merged_tracts[9]) ~ "9",
      Geography %in% unlist(merged_tracts[10]) ~ "10",
      Geography %in% unlist(merged_tracts[11]) ~ "11",
      Geography %in% unlist(merged_tracts[12]) ~ "12",
      Geography %in% unlist(merged_tracts[13]) ~ "13",
      Geography %in% unlist(merged_tracts[14]) ~ "14",
      Geography %in% unlist(merged_tracts[15]) ~ "15",
      Geography %in% unlist(merged_tracts[16]) ~ "16",
      Geography %in% unlist(merged_tracts[17]) ~ "17",
      Geography %in% unlist(merged_tracts[18]) ~ "18",
      Geography %in% unlist(merged_tracts[19]) ~ "19",
      Geography %in% unlist(merged_tracts[20]) ~ "20",
      Geography %in% unlist(merged_tracts[21]) ~ "21",
      Geography %in% unlist(merged_tracts[22]) ~ "22",
      Geography %in% unlist(merged_tracts[23]) ~ "23",
      Geography %in% unlist(merged_tracts[24]) ~ "24",
      Geography %in% unlist(merged_tracts[25]) ~ "25",
      Geography %in% unlist(merged_tracts[26]) ~ "26",
      Geography %in% unlist(merged_tracts[27]) ~ "27",
      Geography %in% unlist(merged_tracts[28]) ~ "28",
      Geography %in% unlist(merged_tracts[29]) ~ "29",
      Geography %in% unlist(merged_tracts[30]) ~ "30",
      Geography %in% unlist(merged_tracts[31]) ~ "31",
      Geography %in% unlist(merged_tracts[32]) ~ "32",
      Geography %in% unlist(merged_tracts[33]) ~ "33",
      Geography %in% unlist(merged_tracts[34]) ~ "34",
      Geography %in% unlist(merged_tracts[35]) ~ "35",
      Geography %in% unlist(merged_tracts[36]) ~ "36",
      Geography %in% unlist(merged_tracts[37]) ~ "37",
      Geography %in% unlist(merged_tracts[38]) ~ "38",
      Geography %in% unlist(merged_tracts[39]) ~ "39",
      Geography %in% unlist(merged_tracts[40]) ~ "40",
      Geography %in% unlist(merged_tracts[41]) ~ "41",
      Geography %in% unlist(merged_tracts[42]) ~ "42",
      Geography %in% unlist(merged_tracts[43]) ~ "43",
      Geography %in% unlist(merged_tracts[44]) ~ "44",
      .default = Geography
    ),
    remove_flag = dplyr::case_when(
      merge %in% c(1:44) & is.na(Population) ~ 1,
      .default = 0
    )) %>%
  dplyr::group_by(merge) %>%
  dplyr::mutate(
    Pop_combined = sum(Population, na.rm = TRUE),
    VAP_MOD = sum(as.numeric(VAP_MOD), na.rm = TRUE),
    G20PREDBID = sum(as.numeric(G20PREDBID), na.rm = TRUE),
    G20PREGHAW = sum(as.numeric(G20PREGHAW), na.rm = TRUE),
    G20PRELJOR = sum(as.numeric(G20PRELJOR), na.rm = TRUE),
    G20PRERTRU = sum(as.numeric(G20PRERTRU), na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(remove_flag != 1) %>%
  dplyr::mutate(
    VAP_MOD = VAP_MOD * Population / Pop_combined,
    G20PREDBID = G20PREDBID * Population / Pop_combined,
    G20PREGHAW = G20PREGHAW * Population / Pop_combined,
    G20PRELJOR = G20PRELJOR * Population / Pop_combined,
    G20PRERTRU = G20PRERTRU * Population / Pop_combined,
  ) %>%
  dplyr::select(-c(merge, Pop_combined)) %>%
  dplyr::mutate(
    missing_district = is.na(district),
    missing_demo = is.na(Population),
    missing_voting = is.na(VAP_MOD),
    missing_data = missing_district == TRUE | missing_demo == TRUE | missing_voting == TRUE
  ) %>%
  dplyr::mutate(across(everything(), ~ ifelse(is.nan(.), 0, .)))

voting_by_district <- full_voting_data %>%
  dplyr::mutate(
    District = factor(district, levels = c(1:16))
  ) %>%
  dplyr::group_by(District, missing_voting) %>%
  dplyr::summarise(
    Population = sum(Population, na.rm = TRUE),
    VAP_MOD = sum(VAP_MOD, na.rm = TRUE),
    `Votes Democratic` = sum(G20PREDBID, na.rm = TRUE),
    `Votes Republican` = sum(G20PRERTRU, na.rm = TRUE),
    `Votes Green` = sum(G20PREGHAW, na.rm = TRUE),
    `Votes Libertarian` = sum(G20PRELJOR, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    `Votes Cast` = `Votes Democratic` + `Votes Republican` + `Votes Green` + `Votes Libertarian`,
    `Votes Two Parties` = `Votes Democratic` + `Votes Republican`,
    `% Votes Democratic (2P)` = 100 * `Votes Democratic` / `Votes Two Parties`,
    `% Votes Republican (2P)` = 100 * `Votes Republican` / `Votes Two Parties`,
    `% Votes Two Parties` = 100 * `Votes Two Parties` / `Votes Cast`,
    `Turnout % (POP)` = 100 * `Votes Cast` / Population,
    `Turnout % (VAP)` = 100 * `Votes Cast` / VAP_MOD,
    `% Margin (2P)` = `% Votes Democratic (2P)` - `% Votes Republican (2P)`,
    Winner = dplyr::case_when(
      `% Margin (2P)` > 0 & missing_voting == FALSE ~ "Democrat",
      `% Margin (2P)` < 0 & missing_voting == FALSE ~ "Republican",
      .default = "No Winner"
    ),
    missing_voting = dplyr::case_when(
      missing_voting == TRUE ~ "Population (missing)",
      missing_voting == FALSE ~ "Population (identified)",
      .default = "Population (missing)"
    )) %>%
  dplyr::group_by(District) %>%
  tidyr::pivot_wider(names_from = missing_voting, values_from = Population) %>%
  dplyr::mutate(
    `Population (identified)` = sum(`Population (identified)`, na.rm = TRUE),
    `Population (missing)` = sum(`Population (missing)`, na.rm = TRUE),
    `Population (total)` = `Population (identified)` + `Population (missing)`,
    `Population (missing) %` = 100 *`Population (missing)` / `Population (total)`
  ) %>%
  dplyr::filter(VAP_MOD != 0) %>%
  dplyr::ungroup() %>%
  # no missing data remains
  dplyr::select(-c(`Population (missing)`, `Population (total)`, `Population (missing) %`)) %>%
  dplyr::rename(Population = `Population (identified)`) %>%
  dplyr::relocate(Population, .after = District)

write.csv(voting_by_district,
          paste0(output_folder,"/partisanship_tracts",output_number,".csv"),
          row.names = FALSE
          )


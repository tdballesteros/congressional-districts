
# https://web.stanford.edu/~jrodden/OH_release/
geo <- read.csv("Data/oh_geo_merged_final.csv")
vote <- read.csv("Data/oh_vote_merged_final.csv")
output <- read.csv("District Outputs Updated/output_vtd01.csv")

votecombo <- dplyr::full_join(vote,output,dplyr::join_by("GEOID10"=="Geography")) %>%
  dplyr::mutate(missing_data_flag = is.na(total_08)) %>%
  dplyr::group_by(district,missing_data_flag) %>%
  dplyr::summarise(
    reg_08 = sum(reg_08, na.rm = TRUE),
    total_08 = sum(total_08, na.rm = TRUE),
    pres_dvote_08 = sum(pres_dvote08, na.rm = TRUE),
    pres_rvote_08 = sum(pres_rvote08, na.rm = TRUE),
    pres_tvote_08 = sum(pres_tvote_08, na.rm = TRUE),
    two_party_vote = pres_dvote_08 + pres_rvote_08
  )

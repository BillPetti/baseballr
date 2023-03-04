## Write Existing List of Season IDs to csv
ncaa_season_id_lu <- load_ncaa_baseball_season_ids()
readr::write_csv(ncaa_season_id_lu, "data-raw/ncaa_season_id_lu.csv")

## Update for following season by checking stats.ncaa.org/team/pages
## 

ncaa_season_id_lu <- readr::read_csv("data-raw/ncaa_season_id_lu.csv")

usethis::use_data(ncaa_season_id_lu, internal = FALSE, overwrite = TRUE)

ncaa_team_lu <- load_ncaa_baseball_teams()

ncaa_team_most_recent <- ncaa_team_lu %>% dplyr::filter(.data$year == 2022)

ncaa_team_most_recent <- ncaa_team_most_recent %>% 
  dplyr::mutate(year = 2023)

ncaa_team_lu <- dplyr::bind_rows(ncaa_team_lu, ncaa_team_most_recent)
ncaa_team_lu <- ncaa_team_lu %>% dplyr::arrange(.data$division, .data$school)
readr::write_csv(ncaa_team_lu, "data-raw/ncaa_team_lu.csv")

usethis::use_data(ncaa_team_lu, internal = FALSE, overwrite = TRUE)
 
ncaa_team_lu <- readr::read_csv("data-raw/ncaa_team_lu.csv")

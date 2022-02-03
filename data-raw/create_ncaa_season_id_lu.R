## Write Existing List of Season IDs to csv
ncaa_season_id_lu <- baseballr::ncaa_season_id_lu
readr::write_csv(ncaa_season_id_lu, "data-raw/ncaa_season_id_lu.csv")

## Update for following season by checking stats.ncaa.org/team/pages
## 

ncaa_season_id_lu <- readr::read_csv("data-raw/ncaa_season_id_lu.csv")

usethis::use_data(ncaa_season_id_lu,internal=FALSE, overwrite = TRUE)
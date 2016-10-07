#' Query Statcast and PITCHf/x Data for Batters from baseballsavant.mlb.com
#'
#' This function allows you to query Statcast and PITCHf/x data as provided on baseballsavant.mlb.com and have that data returned as a dataframe.
#' @param start_date Date of first game for which you want data. Format must be in Y-d-m format.
#' @param end_date Date of last game for which you want data. Format must be in Y-d-m format.
#' @param batterid The MLBAM ID for the batter who's data you want to query.
#' @keywords MLB, sabermetrics, Statcast
#' @export
#' @examples
#' \dontrun{scrape_statcast_savant_batter(start_date = "2016-04-06", end_date = "2016-04-15", batterid = 621043)}

scrape_statcast_savant_batter <- function(start_date, end_date, batterid) {
  print("Be patient, this may take a few seconds...")
  print("Data courtesy of Baseball Savant and MLBAM (baseballsavant.mlb.com)")
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfZ=&hfGT=R%7C&hfPR=&hfAB=&stadium=&hfBBT=&hfBBL=&player_lookup%5B%5D=",batterid,"&hfC=&season=all&player_type=batter&hfOuts=&pitcher_throws=&batter_stands=&start_speed_gt=&start_speed_lt=&perceived_speed_gt=&perceived_speed_lt=&spin_rate_gt=&spin_rate_lt=&exit_velocity_gt=&exit_velocity_lt=&launch_angle_gt=&launch_angle_lt=&distance_gt=&distance_lt=&batted_ball_angle_gt=&batted_ball_angle_lt=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&team=&position=&hfRO=&home_road=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=start_speed&sort_order=desc&min_abs=0&xba_gt=&xba_lt=&px1=&px2=&pz1=&pz2=&ss_gt=&ss_lt=&type=details&")
  x <- read.csv(url)
  x[x=="null"] <- NA
  x$game_date <- as.Date(x$game_date, "%Y-%m-%d")
  x$des <- as.character(x$des)
  x$game_pk <- as.character(x$game_pk) %>% as.numeric()
  x$on_1b <- as.character(x$on_1b) %>% as.numeric()
  x$on_2b <- as.character(x$on_2b) %>% as.numeric()
  x$on_3b <- as.character(x$on_3b) %>% as.numeric()
  x$px <- as.character(x$px) %>% as.numeric()
  x$pz <- as.character(x$pz) %>% as.numeric()
  x$hit_distance_sc <- as.character(x$hit_distance_sc) %>% as.numeric()
  x$hit_speed <- as.character(x$hit_speed) %>% as.numeric()
  x$hit_angle <- as.character(x$hit_angle) %>% as.numeric()
  x$effective_speed <- as.character(x$effective_speed) %>% as.numeric()
  x$release_spin_rate <- as.character(x$release_spin_rate) %>% as.numeric()
  x$release_extension <- as.character(x$release_extension) %>% as.numeric()
  x$barrel <- with(x, ifelse(hit_angle <= 50 & hit_speed >= 98 & hit_speed * 1.5 - hit_angle >= 11 & hit_speed + hit_angle >= 124, 1, 0))
  x
}

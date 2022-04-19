#' @rdname statcast_search
#' @title **Query Statcast by Date Range and Players**
#'
#' @description This function allows you to query Statcast data as provided on \url{https://baseballsavant.mlb.com}
#' @param start_date Date of first game for which you want data.
#' Format must be in YYYY-MM-DD format.
#' @param end_date Date of last game for which you want data.
#' Format must be in YYYY-MM-DD format.
#' @param playerid The MLBAM ID for the player whose data you want to query.
#' @param player_type The player type. Can be \code{batter} or \code{pitcher}.
#' Default is \code{batter}
#' @param ... currently ignored
#' @return Returns a tibble with Statcast data with the following columns:
#' 
#'   |col_name                        |types     |
#'   |:-------------------------------|:---------|
#'   |pitch_type                      |character |
#'   |game_date                       |Date      |
#'   |release_speed                   |numeric   |
#'   |release_pos_x                   |numeric   |
#'   |release_pos_z                   |numeric   |
#'   |player_name                     |character |
#'   |batter                          |numeric   |
#'   |pitcher                         |numeric   |
#'   |events                          |character |
#'   |description                     |character |
#'   |spin_dir                        |logical   |
#'   |spin_rate_deprecated            |logical   |
#'   |break_angle_deprecated          |logical   |
#'   |break_length_deprecated         |logical   |
#'   |zone                            |numeric   |
#'   |des                             |character |
#'   |game_type                       |character |
#'   |stand                           |character |
#'   |p_throws                        |character |
#'   |home_team                       |character |
#'   |away_team                       |character |
#'   |type                            |character |
#'   |hit_location                    |integer   |
#'   |bb_type                         |character |
#'   |balls                           |integer   |
#'   |strikes                         |integer   |
#'   |game_year                       |integer   |
#'   |pfx_x                           |numeric   |
#'   |pfx_z                           |numeric   |
#'   |plate_x                         |numeric   |
#'   |plate_z                         |numeric   |
#'   |on_3b                           |numeric   |
#'   |on_2b                           |numeric   |
#'   |on_1b                           |numeric   |
#'   |outs_when_up                    |integer   |
#'   |inning                          |numeric   |
#'   |inning_topbot                   |character |
#'   |hc_x                            |numeric   |
#'   |hc_y                            |numeric   |
#'   |tfs_deprecated                  |logical   |
#'   |tfs_zulu_deprecated             |logical   |
#'   |fielder_2                       |numeric   |
#'   |umpire                          |logical   |
#'   |sv_id                           |character |
#'   |vx0                             |numeric   |
#'   |vy0                             |numeric   |
#'   |vz0                             |numeric   |
#'   |ax                              |numeric   |
#'   |ay                              |numeric   |
#'   |az                              |numeric   |
#'   |sz_top                          |numeric   |
#'   |sz_bot                          |numeric   |
#'   |hit_distance_sc                 |numeric   |
#'   |launch_speed                    |numeric   |
#'   |launch_angle                    |numeric   |
#'   |effective_speed                 |numeric   |
#'   |release_spin_rate               |numeric   |
#'   |release_extension               |numeric   |
#'   |game_pk                         |numeric   |
#'   |pitcher_1                       |numeric   |
#'   |fielder_2_1                     |numeric   |
#'   |fielder_3                       |numeric   |
#'   |fielder_4                       |numeric   |
#'   |fielder_5                       |numeric   |
#'   |fielder_6                       |numeric   |
#'   |fielder_7                       |numeric   |
#'   |fielder_8                       |numeric   |
#'   |fielder_9                       |numeric   |
#'   |release_pos_y                   |numeric   |
#'   |estimated_ba_using_speedangle   |numeric   |
#'   |estimated_woba_using_speedangle |numeric   |
#'   |woba_value                      |numeric   |
#'   |woba_denom                      |integer   |
#'   |babip_value                     |integer   |
#'   |iso_value                       |integer   |
#'   |launch_speed_angle              |integer   |
#'   |at_bat_number                   |numeric   |
#'   |pitch_number                    |numeric   |
#'   |pitch_name                      |character |
#'   |home_score                      |numeric   |
#'   |away_score                      |numeric   |
#'   |bat_score                       |numeric   |
#'   |fld_score                       |numeric   |
#'   |post_away_score                 |numeric   |
#'   |post_home_score                 |numeric   |
#'   |post_bat_score                  |numeric   |
#'   |post_fld_score                  |numeric   |
#'   |if_fielding_alignment           |character |
#'   |of_fielding_alignment           |character |
#'   |spin_axis                       |numeric   |
#'   |delta_home_win_exp              |numeric   |
#'   |delta_run_exp                   |numeric   |
#' 
#' @importFrom tibble tribble
#' @importFrom lubridate year
#' @export
#' @examples
#' \donttest{
#'   ### Correa
#'   x<- try(statcast_search(start_date = "2016-04-06",
#'                       end_date = "2016-04-15", 
#'                       playerid = 621043, 
#'                       player_type = 'batter'))
#'   ### Noah
#'   try(statcast_search(start_date = "2016-04-06",
#'                       end_date = "2016-04-15", 
#'                       playerid = 592789, 
#'                       player_type = 'pitcher'))
#'   ### Daily
#'   try(statcast_search(start_date = "2016-04-06", 
#'                       end_date = "2016-04-06"))
#' }

statcast_search <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
                                   playerid = NULL,
                                   player_type = "batter", ...) {
  # Check for other user errors.
  if (start_date <= "2015-03-01") { # March 1, 2015 was the first date of Spring Training.
    message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
  }
  if (start_date < "2008-03-25") { # March 25, 2008 was the first date of the 2008 season.
    stop("The data are limited to the 2008 MLB season and after.")
    return(NULL)
  }
  if (start_date == Sys.Date()) {
    message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
  }
  if (start_date > as.Date(end_date)) {
    stop("The start date is later than the end date.")
    return(NULL)
  }

  playerid_var <- ifelse(player_type == "pitcher",
                         "pitchers_lookup%5B%5D", "batters_lookup%5B%5D")

  vars <- tibble::tribble(
    ~var, ~value,
    "all", "true",
    "hfPT", "",
    "hfAB", "",
    "hfBBT", "",
    "hfPR", "",
    "hfZ", "",
    "stadium", "",
    "hfBBL", "",
    "hfNewZones", "",
    "hfGT", "R%7CPO%7CS%7C&hfC",
    "hfSea", paste0(lubridate::year(start_date), "%7C"),
    "hfSit", "",
    "hfOuts", "",
    "opponent", "",
    "pitcher_throws", "",
    "batter_stands", "",
    "hfSA", "",
    "player_type", player_type,
    "hfInfield", "",
    "team", "",
    "position", "",
    "hfOutfield", "",
    "hfRO", "",
    "home_road", "",
    playerid_var, ifelse(is.null(playerid), "", as.character(playerid)),
    "game_date_gt", as.character(start_date),
    "game_date_lt", as.character(end_date),
    "hfFlag", "",
    "hfPull", "",
    "metric_1", "",
    "hfInn", "",
    "min_pitches", "0",
    "min_results", "0",
    "group_by", "name",
    "sort_col", "pitches",
    "player_event_sort", "h_launch_speed",
    "sort_order", "desc",
    "min_abs", "0",
    "type", "details") %>%
    dplyr::mutate(pairs = paste0(.data$var, "=", .data$value))

  if (is.null(playerid)) {
    # message("No playerid specified. Collecting data for all batters/pitchers.")
    vars <- vars %>% 
      dplyr::filter(!grepl("lookup", .data$var))
  }

  url_vars <- paste0(vars$pairs, collapse = "&")
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?", url_vars)
  # message(url)

  # Do a try/catch to show errors that the user may encounter while downloading.
  tryCatch(
    { 
      suppressMessages(
        suppressWarnings(
          payload <- csv_from_url(url)
        )
      )
    },
    error = function(cond) {
      message(cond)
      stop("No payload acquired")
    },
    # this will never run??
    warning = function(cond) {
      message(cond)
    }
  )
  # returns 0 rows on failure but > 1 columns
  if (nrow(payload) > 1) {

    names(payload) <- c("pitch_type", "game_date", "release_speed", "release_pos_x",
                        "release_pos_z", "player_name", "batter", "pitcher", "events",
                        "description", "spin_dir", "spin_rate_deprecated", "break_angle_deprecated",
                        "break_length_deprecated", "zone", "des", "game_type", "stand",
                        "p_throws", "home_team", "away_team", "type", "hit_location",
                        "bb_type", "balls", "strikes", "game_year", "pfx_x", "pfx_z",
                        "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up",
                        "inning", "inning_topbot", "hc_x", "hc_y", "tfs_deprecated",
                        "tfs_zulu_deprecated", "fielder_2", "umpire", "sv_id", "vx0",
                        "vy0", "vz0", "ax", "ay", "az", "sz_top", "sz_bot", "hit_distance_sc",
                        "launch_speed", "launch_angle", "effective_speed", "release_spin_rate",
                        "release_extension", "game_pk", "pitcher_1", "fielder_2_1",
                        "fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                        "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle",
                        "estimated_woba_using_speedangle", "woba_value", "woba_denom",
                        "babip_value", "iso_value", "launch_speed_angle", "at_bat_number",
                        "pitch_number", "pitch_name", "home_score", "away_score", "bat_score",
                        "fld_score", "post_away_score", "post_home_score", "post_bat_score",
                        "post_fld_score", "if_fielding_alignment", "of_fielding_alignment",
                        "spin_axis", "delta_home_win_exp", "delta_run_exp")
    payload <- process_statcast_payload(payload) %>%
      make_baseballr_data("MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com",Sys.time())
    return(payload)
  } else {
    warning("No valid data found")

    names(payload) <- c("pitch_type", "game_date", "release_speed", "release_pos_x",
                        "release_pos_z", "player_name", "batter", "pitcher", "events",
                        "description", "spin_dir", "spin_rate_deprecated", "break_angle_deprecated",
                        "break_length_deprecated", "zone", "des", "game_type", "stand",
                        "p_throws", "home_team", "away_team", "type", "hit_location",
                        "bb_type", "balls", "strikes", "game_year", "pfx_x", "pfx_z",
                        "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up",
                        "inning", "inning_topbot", "hc_x", "hc_y", "tfs_deprecated",
                        "tfs_zulu_deprecated", "fielder_2", "umpire", "sv_id", "vx0",
                        "vy0", "vz0", "ax", "ay", "az", "sz_top", "sz_bot", "hit_distance_sc",
                        "launch_speed", "launch_angle", "effective_speed", "release_spin_rate",
                        "release_extension", "game_pk", "pitcher_1", "fielder_2_1",
                        "fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                        "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle",
                        "estimated_woba_using_speedangle", "woba_value", "woba_denom",
                        "babip_value", "iso_value", "launch_speed_angle", "at_bat_number",
                        "pitch_number", "pitch_name", "home_score", "away_score", "bat_score",
                        "fld_score", "post_away_score", "post_home_score", "post_bat_score",
                        "post_fld_score", "if_fielding_alignment", "of_fielding_alignment",
                        "spin_axis", "delta_home_win_exp", "delta_run_exp")
    payload <- payload %>%
      make_baseballr_data("MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com",Sys.time())
    return(payload)
  }
}

#' @rdname statcast_search
#' @return Returns a tibble with Statcast data.
#' @export
statcast_search.default <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
                                             playerid = NULL, player_type = "batter", ...) {
  # Check to make sure args are in the correct format.
  # if(!is.character(start_date) | !is.character(end_date)) {
  #   warning("Please wrap your dates in quotations in 'yyyy-mm-dd' format.")
  #   return(NULL)
  # }
  message(paste0(start_date, " is not a date. Attempting to coerce..."))
  start_Date <- as.Date(start_date)

  tryCatch(
    {
      end_Date <- as.Date(end_date)
    },
    warning = function(cond) {
      message(paste0(end_date, " was not coercible into a date. Using today."))
      end_Date <- Sys.Date()
      message("Original warning message:")
      message(cond)
    }
  )

  statcast_search(start_Date, end_Date,
                         playerid, player_type, ...)

}

#' @rdname statcast_search
#' @param start_date Date of first game for which you want data.
#' Format must be in YYYY-MM-DD format.
#' @param end_date Date of last game for which you want data.
#' Format must be in YYYY-MM-DD format.
#' @param batterid The MLBAM ID for the batter whose data you want to query.
#' @return Returns a tibble with Statcast data with the following columns:
#' 
#'   |col_name                        |types     |
#'   |:-------------------------------|:---------|
#'   |pitch_type                      |character |
#'   |game_date                       |Date      |
#'   |release_speed                   |numeric   |
#'   |release_pos_x                   |numeric   |
#'   |release_pos_z                   |numeric   |
#'   |player_name                     |character |
#'   |batter                          |numeric   |
#'   |pitcher                         |numeric   |
#'   |events                          |character |
#'   |description                     |character |
#'   |spin_dir                        |logical   |
#'   |spin_rate_deprecated            |logical   |
#'   |break_angle_deprecated          |logical   |
#'   |break_length_deprecated         |logical   |
#'   |zone                            |numeric   |
#'   |des                             |character |
#'   |game_type                       |character |
#'   |stand                           |character |
#'   |p_throws                        |character |
#'   |home_team                       |character |
#'   |away_team                       |character |
#'   |type                            |character |
#'   |hit_location                    |integer   |
#'   |bb_type                         |character |
#'   |balls                           |integer   |
#'   |strikes                         |integer   |
#'   |game_year                       |integer   |
#'   |pfx_x                           |numeric   |
#'   |pfx_z                           |numeric   |
#'   |plate_x                         |numeric   |
#'   |plate_z                         |numeric   |
#'   |on_3b                           |numeric   |
#'   |on_2b                           |numeric   |
#'   |on_1b                           |numeric   |
#'   |outs_when_up                    |integer   |
#'   |inning                          |numeric   |
#'   |inning_topbot                   |character |
#'   |hc_x                            |numeric   |
#'   |hc_y                            |numeric   |
#'   |tfs_deprecated                  |logical   |
#'   |tfs_zulu_deprecated             |logical   |
#'   |fielder_2                       |numeric   |
#'   |umpire                          |logical   |
#'   |sv_id                           |character |
#'   |vx0                             |numeric   |
#'   |vy0                             |numeric   |
#'   |vz0                             |numeric   |
#'   |ax                              |numeric   |
#'   |ay                              |numeric   |
#'   |az                              |numeric   |
#'   |sz_top                          |numeric   |
#'   |sz_bot                          |numeric   |
#'   |hit_distance_sc                 |numeric   |
#'   |launch_speed                    |numeric   |
#'   |launch_angle                    |numeric   |
#'   |effective_speed                 |numeric   |
#'   |release_spin_rate               |numeric   |
#'   |release_extension               |numeric   |
#'   |game_pk                         |numeric   |
#'   |pitcher_1                       |numeric   |
#'   |fielder_2_1                     |numeric   |
#'   |fielder_3                       |numeric   |
#'   |fielder_4                       |numeric   |
#'   |fielder_5                       |numeric   |
#'   |fielder_6                       |numeric   |
#'   |fielder_7                       |numeric   |
#'   |fielder_8                       |numeric   |
#'   |fielder_9                       |numeric   |
#'   |release_pos_y                   |numeric   |
#'   |estimated_ba_using_speedangle   |numeric   |
#'   |estimated_woba_using_speedangle |numeric   |
#'   |woba_value                      |numeric   |
#'   |woba_denom                      |integer   |
#'   |babip_value                     |integer   |
#'   |iso_value                       |integer   |
#'   |launch_speed_angle              |integer   |
#'   |at_bat_number                   |numeric   |
#'   |pitch_number                    |numeric   |
#'   |pitch_name                      |character |
#'   |home_score                      |numeric   |
#'   |away_score                      |numeric   |
#'   |bat_score                       |numeric   |
#'   |fld_score                       |numeric   |
#'   |post_away_score                 |numeric   |
#'   |post_home_score                 |numeric   |
#'   |post_bat_score                  |numeric   |
#'   |post_fld_score                  |numeric   |
#'   |if_fielding_alignment           |character |
#'   |of_fielding_alignment           |character |
#'   |spin_axis                       |numeric   |
#'   |delta_home_win_exp              |numeric   |
#'   |delta_run_exp                   |numeric   |
#'   
#' @export
#' @examples
#' \donttest{
#'   correa <- statcast_search_batters(start_date = "2016-04-06",
#'     end_date = "2016-04-15", batterid = 621043)
#'   daily <- statcast_search_batters(start_date = "2016-04-06",
#'     end_date = "2016-04-06", batterid = NULL)
#' }

statcast_search_batters <- function(start_date, end_date, batterid = NULL, ...) {
  statcast_search(start_date, end_date, playerid = batterid,
                         player_type = "batter", ...)
}


#' @rdname statcast_search
#' @param pitcherid The MLBAM ID for the pitcher whose data you want to query.
#' @return Returns a tibble with Statcast data with the following columns:
#' 
#'   |col_name                        |types     |
#'   |:-------------------------------|:---------|
#'   |pitch_type                      |character |
#'   |game_date                       |Date      |
#'   |release_speed                   |numeric   |
#'   |release_pos_x                   |numeric   |
#'   |release_pos_z                   |numeric   |
#'   |player_name                     |character |
#'   |batter                          |numeric   |
#'   |pitcher                         |numeric   |
#'   |events                          |character |
#'   |description                     |character |
#'   |spin_dir                        |logical   |
#'   |spin_rate_deprecated            |logical   |
#'   |break_angle_deprecated          |logical   |
#'   |break_length_deprecated         |logical   |
#'   |zone                            |numeric   |
#'   |des                             |character |
#'   |game_type                       |character |
#'   |stand                           |character |
#'   |p_throws                        |character |
#'   |home_team                       |character |
#'   |away_team                       |character |
#'   |type                            |character |
#'   |hit_location                    |integer   |
#'   |bb_type                         |character |
#'   |balls                           |integer   |
#'   |strikes                         |integer   |
#'   |game_year                       |integer   |
#'   |pfx_x                           |numeric   |
#'   |pfx_z                           |numeric   |
#'   |plate_x                         |numeric   |
#'   |plate_z                         |numeric   |
#'   |on_3b                           |numeric   |
#'   |on_2b                           |numeric   |
#'   |on_1b                           |numeric   |
#'   |outs_when_up                    |integer   |
#'   |inning                          |numeric   |
#'   |inning_topbot                   |character |
#'   |hc_x                            |numeric   |
#'   |hc_y                            |numeric   |
#'   |tfs_deprecated                  |logical   |
#'   |tfs_zulu_deprecated             |logical   |
#'   |fielder_2                       |numeric   |
#'   |umpire                          |logical   |
#'   |sv_id                           |character |
#'   |vx0                             |numeric   |
#'   |vy0                             |numeric   |
#'   |vz0                             |numeric   |
#'   |ax                              |numeric   |
#'   |ay                              |numeric   |
#'   |az                              |numeric   |
#'   |sz_top                          |numeric   |
#'   |sz_bot                          |numeric   |
#'   |hit_distance_sc                 |numeric   |
#'   |launch_speed                    |numeric   |
#'   |launch_angle                    |numeric   |
#'   |effective_speed                 |numeric   |
#'   |release_spin_rate               |numeric   |
#'   |release_extension               |numeric   |
#'   |game_pk                         |numeric   |
#'   |pitcher_1                       |numeric   |
#'   |fielder_2_1                     |numeric   |
#'   |fielder_3                       |numeric   |
#'   |fielder_4                       |numeric   |
#'   |fielder_5                       |numeric   |
#'   |fielder_6                       |numeric   |
#'   |fielder_7                       |numeric   |
#'   |fielder_8                       |numeric   |
#'   |fielder_9                       |numeric   |
#'   |release_pos_y                   |numeric   |
#'   |estimated_ba_using_speedangle   |numeric   |
#'   |estimated_woba_using_speedangle |numeric   |
#'   |woba_value                      |numeric   |
#'   |woba_denom                      |integer   |
#'   |babip_value                     |integer   |
#'   |iso_value                       |integer   |
#'   |launch_speed_angle              |integer   |
#'   |at_bat_number                   |numeric   |
#'   |pitch_number                    |numeric   |
#'   |pitch_name                      |character |
#'   |home_score                      |numeric   |
#'   |away_score                      |numeric   |
#'   |bat_score                       |numeric   |
#'   |fld_score                       |numeric   |
#'   |post_away_score                 |numeric   |
#'   |post_home_score                 |numeric   |
#'   |post_bat_score                  |numeric   |
#'   |post_fld_score                  |numeric   |
#'   |if_fielding_alignment           |character |
#'   |of_fielding_alignment           |character |
#'   |spin_axis                       |numeric   |
#'   |delta_home_win_exp              |numeric   |
#'   |delta_run_exp                   |numeric   |
#' 
#' @export
#' @examples
#' \donttest{
#'   x <- statcast_search_pitchers(start_date = "2016-04-06",
#'     end_date = "2016-04-15", pitcherid = 592789)
#'   daily <- statcast_search_pitchers(start_date = "2016-04-06",
#'     end_date = "2016-04-06", pitcherid = NULL)
#' }

statcast_search_pitchers <- function(start_date, end_date, pitcherid = NULL, ...) {
  statcast_search(start_date, end_date, playerid = pitcherid,
                  player_type = "pitcher", ...)
}


#' @rdname scrape_statcast_savant
#' @title **(legacy) Query Statcast by Date Range and Players**
#' @inheritParams statcast_search
#' @return Returns a tibble with Statcast data.
#' @keywords legacy
#' @export
scrape_statcast_savant <- statcast_search

#' @rdname scrape_statcast_savant
#' @return Returns a tibble with Statcast data.
#' @keywords legacy
#' @export
scrape_statcast_savant.Date <- statcast_search


#' @rdname scrape_statcast_savant
#' @return Returns a tibble with Statcast data.
#' @keywords legacy
#' @export
scrape_statcast_savant.default <- statcast_search.default

#' @rdname scrape_statcast_savant
#' @return Returns a tibble with Statcast data.
#' @keywords legacy
#' @export
scrape_statcast_savant_batter <- statcast_search_batters


#' @rdname scrape_statcast_savant
#' @return Returns a tibble with Statcast data.
#' @keywords legacy
#' @export
scrape_statcast_savant_batter_all <- statcast_search_batters



#' @rdname scrape_statcast_savant
#' @return Returns a tibble with Statcast data.
#' @keywords legacy
#' @export
scrape_statcast_savant_pitcher <- statcast_search_pitchers


#' @rdname scrape_statcast_savant
#' @return Returns a tibble with Statcast data.
#' @keywords legacy
#' @export
scrape_statcast_savant_pitcher_all <- statcast_search_pitchers

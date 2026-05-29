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
#'   |col_name                                     |types     |description |
#'   |:--------------------------------------------|:---------|:-----------|
#'   |pitch_type                                   |character |Abbreviation of the pitch type thrown (e.g. FF, SL, CH). |
#'   |game_date                                    |Date      |Date the game was played. |
#'   |release_speed                                |numeric   |Pitch velocity out of the hand (mph). |
#'   |release_pos_x                                |numeric   |Horizontal release position of the ball, catcher's perspective (feet). |
#'   |release_pos_z                                |numeric   |Vertical release position of the ball, catcher's perspective (feet). |
#'   |player_name                                  |character |Pitcher (or batter, by query) name, Last, First. |
#'   |batter                                       |numeric   |MLBAM ID for the batter. |
#'   |pitcher                                      |numeric   |MLBAM ID for the pitcher. |
#'   |events                                       |character |Plate-appearance outcome (e.g. single, strikeout, home_run). |
#'   |description                                  |character |Pitch-level result description (e.g. ball, called_strike, hit_into_play). |
#'   |spin_dir                                     |logical   |Deprecated spin direction field, no longer populated. |
#'   |spin_rate_deprecated                         |logical   |Deprecated legacy spin-rate field, no longer populated. |
#'   |break_angle_deprecated                       |logical   |Deprecated legacy break-angle field, no longer populated. |
#'   |break_length_deprecated                      |logical   |Deprecated legacy break-length field, no longer populated. |
#'   |zone                                         |numeric   |Strike-zone region the pitch crossed (1-14 Gameday zone). |
#'   |des                                          |character |Full text description of the play. |
#'   |game_type                                    |character |Type of game: R (regular), PO/F/D/L/W (postseason), S (spring). |
#'   |stand                                        |character |Side of the plate the batter is standing (L or R). |
#'   |p_throws                                     |character |Hand the pitcher throws with (L or R). |
#'   |home_team                                    |character |Home team abbreviation. |
#'   |away_team                                    |character |Away team abbreviation. |
#'   |type                                         |character |Pitch result code: B (ball), S (strike), X (in play). |
#'   |hit_location                                 |integer   |Fielder position number that fielded the ball. |
#'   |bb_type                                      |character |Batted-ball type (ground_ball, line_drive, fly_ball, popup). |
#'   |balls                                        |integer   |Ball count before the pitch. |
#'   |strikes                                      |integer   |Strike count before the pitch. |
#'   |game_year                                    |integer   |Season year of the game. |
#'   |pfx_x                                        |numeric   |Horizontal pitch movement from the catcher's perspective (feet). |
#'   |pfx_z                                        |numeric   |Vertical pitch movement from the catcher's perspective (feet). |
#'   |plate_x                                      |numeric   |Horizontal position of the pitch crossing the plate (feet from center). |
#'   |plate_z                                      |numeric   |Vertical position of the pitch crossing the plate (feet above ground). |
#'   |on_3b                                        |numeric   |MLBAM ID of the runner on third base, if any. |
#'   |on_2b                                        |numeric   |MLBAM ID of the runner on second base, if any. |
#'   |on_1b                                        |numeric   |MLBAM ID of the runner on first base, if any. |
#'   |outs_when_up                                 |integer   |Number of outs when the batter came to the plate. |
#'   |inning                                       |numeric   |Inning number. |
#'   |inning_topbot                                |character |Half of the inning (Top or Bot). |
#'   |hc_x                                         |numeric   |Hit coordinate X on the field diagram. |
#'   |hc_y                                         |numeric   |Hit coordinate Y on the field diagram. |
#'   |tfs_deprecated                               |logical   |Deprecated time-from-start field, no longer populated. |
#'   |tfs_zulu_deprecated                          |logical   |Deprecated Zulu time-from-start field, no longer populated. |
#'   |umpire                                       |logical   |Deprecated umpire field, no longer populated. |
#'   |sv_id                                        |logical   |Deprecated Sportvision/Statcast pitch identifier, no longer populated. |
#'   |vx0                                          |numeric   |Velocity of the pitch in the x-direction at y=50 ft (ft/s). |
#'   |vy0                                          |numeric   |Velocity of the pitch in the y-direction at y=50 ft (ft/s). |
#'   |vz0                                          |numeric   |Velocity of the pitch in the z-direction at y=50 ft (ft/s). |
#'   |ax                                           |numeric   |Acceleration of the pitch in the x-direction at y=50 ft (ft/s^2). |
#'   |ay                                           |numeric   |Acceleration of the pitch in the y-direction at y=50 ft (ft/s^2). |
#'   |az                                           |numeric   |Acceleration of the pitch in the z-direction at y=50 ft (ft/s^2). |
#'   |sz_top                                       |numeric   |Top of the batter's strike zone for the pitch (feet). |
#'   |sz_bot                                       |numeric   |Bottom of the batter's strike zone for the pitch (feet). |
#'   |hit_distance_sc                              |numeric   |Statcast-measured projected distance of the batted ball (feet). |
#'   |launch_speed                                 |numeric   |Exit velocity of the batted ball (mph). |
#'   |launch_angle                                 |numeric   |Vertical launch angle of the batted ball (degrees). |
#'   |effective_speed                              |numeric   |Perceived velocity adjusted for release extension (mph). |
#'   |release_spin_rate                            |numeric   |Spin rate of the pitch at release (rpm). |
#'   |release_extension                            |numeric   |Distance toward the plate at release (feet). |
#'   |game_pk                                      |numeric   |Unique MLB game identifier. |
#'   |fielder_2                                    |numeric   |MLBAM ID of the catcher. |
#'   |fielder_3                                    |numeric   |MLBAM ID of the first baseman. |
#'   |fielder_4                                    |numeric   |MLBAM ID of the second baseman. |
#'   |fielder_5                                    |numeric   |MLBAM ID of the third baseman. |
#'   |fielder_6                                    |numeric   |MLBAM ID of the shortstop. |
#'   |fielder_7                                    |numeric   |MLBAM ID of the left fielder. |
#'   |fielder_8                                    |numeric   |MLBAM ID of the center fielder. |
#'   |fielder_9                                    |numeric   |MLBAM ID of the right fielder. |
#'   |release_pos_y                                |numeric   |Release position of the ball toward the plate (feet). |
#'   |estimated_ba_using_speedangle                |numeric   |Expected batting average based on exit velocity and launch angle. |
#'   |estimated_woba_using_speedangle              |numeric   |Expected wOBA based on exit velocity and launch angle. |
#'   |woba_value                                   |numeric   |wOBA value assigned to the event. |
#'   |woba_denom                                   |integer   |wOBA denominator (plate-appearance weight) for the event. |
#'   |babip_value                                  |integer   |BABIP value assigned to the event (0 or 1). |
#'   |iso_value                                    |integer   |Isolated power value assigned to the event. |
#'   |launch_speed_angle                           |integer   |Batted-ball classification code (1-6) from exit velocity and angle. |
#'   |at_bat_number                                |numeric   |Sequential plate-appearance number within the game. |
#'   |pitch_number                                 |numeric   |Pitch number within the plate appearance. |
#'   |pitch_name                                   |character |Full name of the pitch type (e.g. 4-Seam Fastball, Slider). |
#'   |home_score                                   |numeric   |Home team score before the pitch. |
#'   |away_score                                   |numeric   |Away team score before the pitch. |
#'   |bat_score                                    |numeric   |Batting team score before the pitch. |
#'   |fld_score                                    |numeric   |Fielding team score before the pitch. |
#'   |post_away_score                              |numeric   |Away team score after the pitch. |
#'   |post_home_score                              |numeric   |Home team score after the pitch. |
#'   |post_bat_score                               |numeric   |Batting team score after the pitch. |
#'   |post_fld_score                               |numeric   |Fielding team score after the pitch. |
#'   |if_fielding_alignment                        |character |Infield defensive alignment (Standard, Strategic, Infield shift). |
#'   |of_fielding_alignment                        |character |Outfield defensive alignment (Standard, Strategic, 4th outfielder). |
#'   |spin_axis                                    |numeric   |Spin axis of the pitch as a clock-face angle (degrees). |
#'   |delta_home_win_exp                           |numeric   |Change in home team win expectancy on the play. |
#'   |delta_run_exp                                |numeric   |Change in run expectancy on the play. |
#'   |bat_speed                                    |numeric   |Bat speed at the point of contact (mph). |
#'   |swing_length                                 |numeric   |Length of the swing path to contact (feet). |
#'   |estimated_slg_using_speedangle               |numeric   |Expected slugging based on exit velocity and launch angle. |
#'   |delta_pitcher_run_exp                        |numeric   |Change in run expectancy credited to the pitcher. |
#'   |hyper_speed                                  |numeric   |Adjusted (90th-percentile) exit velocity (mph). |
#'   |home_score_diff                              |integer   |Home team score minus away team score before the pitch. |
#'   |bat_score_diff                               |integer   |Batting team score minus fielding team score before the pitch. |
#'   |home_win_exp                                 |numeric   |Home team win expectancy before the play. |
#'   |bat_win_exp                                  |numeric   |Batting team win expectancy before the play. |
#'   |age_pit_legacy                               |integer   |Pitcher age using the legacy calculation. |
#'   |age_bat_legacy                               |integer   |Batter age using the legacy calculation. |
#'   |age_pit                                      |integer   |Pitcher age for the season. |
#'   |age_bat                                      |integer   |Batter age for the season. |
#'   |n_thruorder_pitcher                          |integer   |Times through the order the pitcher is facing the lineup. |
#'   |n_priorpa_thisgame_player_at_bat             |integer   |Number of prior plate appearances by the batter in the game. |
#'   |pitcher_days_since_prev_game                 |integer   |Days since the pitcher's previous game appearance. |
#'   |batter_days_since_prev_game                  |integer   |Days since the batter's previous game appearance. |
#'   |pitcher_days_until_next_game                 |integer   |Days until the pitcher's next game appearance. |
#'   |batter_days_until_next_game                  |integer   |Days until the batter's next game appearance. |
#'   |api_break_z_with_gravity                     |numeric   |Vertical pitch break including gravity (inches). |
#'   |api_break_x_arm                              |numeric   |Horizontal pitch break to the pitcher's arm side (inches). |
#'   |api_break_x_batter_in                        |numeric   |Horizontal pitch break toward/away from the batter (inches). |
#'   |arm_angle                                    |numeric   |Pitcher's arm angle at release (degrees). |
#'   |attack_angle                                 |numeric   |Angle of the bat's path at contact (degrees). |
#'   |attack_direction                             |numeric   |Horizontal direction of the swing at contact (degrees). |
#'   |swing_path_tilt                              |numeric   |Vertical tilt of the swing path (degrees). |
#'   |intercept_ball_minus_batter_pos_x_inches     |numeric   |Horizontal offset of ball-bat intercept from batter position (inches). |
#'   |intercept_ball_minus_batter_pos_y_inches     |numeric   |Depth offset of ball-bat intercept from batter position (inches). |
#'
#' @importFrom tibble tribble
#' @importFrom lubridate year
#' @export
#' @examples
#' \donttest{
#'   ### Harper
#'   try(statcast_search(start_date = "2022-10-06", 
#'                       end_date = "2022-10-16", 
#'                       playerid = 547180, 
#'                       player_type = 'batter'))
#'   ### Framber
#'   try(statcast_search(start_date = "2022-10-06", 
#'                       end_date = "2022-10-16", 
#'                       playerid = 664285, 
#'                       player_type = 'pitcher'))
#'   ### Daily
#'   try(statcast_search(start_date = "2022-11-04", 
#'                       end_date = "2022-11-06"))
#' }

statcast_search <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
                            playerid = NULL,
                            player_type = "batter", ...) {
  # Check for other user errors.
  if (start_date <= "2015-03-01") { # March 1, 2015 was the first date of Spring Training.
    cli::cli_alert_info("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
  }
  if (start_date < "2008-03-25") { # March 25, 2008 was the first date of the 2008 season.
    stop("The data are limited to the 2008 MLB season and after.")
    return(NULL)
  }
  if (start_date == Sys.Date()) {
    cli::cli_alert_info("The data are collected daily at 3 a.m. Some of today's games may not be included.")
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
    "type", "details") |>
    dplyr::mutate(pairs = paste0(.data$var, "=", .data$value))
  
  if (is.null(playerid)) {
    # message("No playerid specified. Collecting data for all batters/pitchers.")
    vars <- vars |> 
      dplyr::filter(!grepl("lookup", .data$var))
  }
  
  url_vars <- paste0(vars$pairs, collapse = "&")
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?", url_vars)
  # message(url)
  
  # Do a try/catch to show errors that the user may encounter while downloading.
  payload <- NULL
  tryCatch(
    { 
      suppressMessages(
        suppressWarnings(
          payload <- csv_from_url(url, encoding ="UTF-8")
        )
      )
    },
    error = function(cond) {
      cli::cli_alert_danger("{conditionMessage(cond)}")
      stop("No payload acquired")
    },
    # this will never run??
    warning = function(cond) {
      cli::cli_alert_warning("{conditionMessage(cond)}")
    }
  )
  # Baseball Savant periodically appends new columns to the CSV export (for
  # example bat_speed/swing_length, arm_angle, and the attack_* family). The
  # download already carries a header row, so rather than overwrite every name
  # from a fixed-length vector -- which errors the moment the column count
  # changes ("can't assign N names to an M column data.table", #337, #354,
  # #371, #390) -- assign the known names positionally and leave any extra
  # trailing columns under the names Savant already supplied.
  statcast_columns <- c(
    "pitch_type", "game_date", "release_speed", "release_pos_x",
    "release_pos_z", "player_name", "batter", "pitcher",
    "events", "description", "spin_dir", "spin_rate_deprecated",
    "break_angle_deprecated", "break_length_deprecated", "zone", "des",
    "game_type", "stand", "p_throws", "home_team",
    "away_team", "type", "hit_location", "bb_type",
    "balls", "strikes", "game_year", "pfx_x",
    "pfx_z", "plate_x", "plate_z", "on_3b",
    "on_2b", "on_1b", "outs_when_up", "inning",
    "inning_topbot", "hc_x", "hc_y", "tfs_deprecated",
    "tfs_zulu_deprecated", "umpire", "sv_id", "vx0",
    "vy0", "vz0", "ax", "ay",
    "az", "sz_top", "sz_bot", "hit_distance_sc",
    "launch_speed", "launch_angle", "effective_speed", "release_spin_rate",
    "release_extension", "game_pk", "fielder_2", "fielder_3",
    "fielder_4", "fielder_5", "fielder_6", "fielder_7",
    "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle",
    "estimated_woba_using_speedangle", "woba_value", "woba_denom", "babip_value",
    "iso_value", "launch_speed_angle", "at_bat_number", "pitch_number",
    "pitch_name", "home_score", "away_score", "bat_score",
    "fld_score", "post_away_score", "post_home_score", "post_bat_score",
    "post_fld_score", "if_fielding_alignment", "of_fielding_alignment", "spin_axis",
    "delta_home_win_exp", "delta_run_exp", "bat_speed", "swing_length",
    "estimated_slg_using_speedangle", "delta_pitcher_run_exp", "hyper_speed", "home_score_diff",
    "bat_score_diff", "home_win_exp", "bat_win_exp", "age_pit_legacy",
    "age_bat_legacy", "age_pit", "age_bat", "n_thruorder_pitcher",
    "n_priorpa_thisgame_player_at_bat", "pitcher_days_since_prev_game", "batter_days_since_prev_game", "pitcher_days_until_next_game",
    "batter_days_until_next_game", "api_break_z_with_gravity", "api_break_x_arm", "api_break_x_batter_in",
    "arm_angle", "attack_angle", "attack_direction", "swing_path_tilt",
    "intercept_ball_minus_batter_pos_x_inches", "intercept_ball_minus_batter_pos_y_inches"
  )
  n_known <- min(length(statcast_columns), ncol(payload))
  names(payload)[seq_len(n_known)] <- statcast_columns[seq_len(n_known)]

  # returns 0 rows on failure but > 1 columns
  if (nrow(payload) > 1) {
    payload <- process_statcast_payload(payload) |>
      make_baseballr_data("MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com", Sys.time())
  } else {
    cli::cli_warn("No valid data found")
    payload <- payload |>
      make_baseballr_data("MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com", Sys.time())
  }
  return(payload)
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
  cli::cli_alert_warning(paste0(start_date, " is not a date. Attempting to coerce..."))
  start_Date <- as.Date(start_date)
  
  tryCatch(
    {
      end_Date <- as.Date(end_date)
    },
    warning = function(cond) {
      cli::cli_alert_warning(paste0(end_date, " was not coercible into a date. Using today."))
      end_Date <- Sys.Date()
      cli::cli_alert_warning("Original warning message:")
      cli::cli_alert_warning("{conditionMessage(cond)}")
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
#' @return Returns a tibble with the same Statcast pitch-level columns as
#'   [statcast_search()], filtered to the requested batter. See the
#'   [statcast_search()] return value for the full column-by-column reference.
#' @export
#' @examples
#' \donttest{
#'   try({
#'     correa <- statcast_search_batters(start_date = "2016-04-06",
#'       end_date = "2016-04-15", batterid = 621043)
#'     daily <- statcast_search_batters(start_date = "2016-04-06",
#'       end_date = "2016-04-06", batterid = NULL)
#'   })
#' }

statcast_search_batters <- function(start_date, end_date, batterid = NULL, ...) {
  statcast_search(start_date, end_date, playerid = batterid,
                  player_type = "batter", ...)
}


#' @rdname statcast_search
#' @param pitcherid The MLBAM ID for the pitcher whose data you want to query.
#' @return Returns a tibble with the same Statcast pitch-level columns as
#'   [statcast_search()], filtered to the requested pitcher. See the
#'   [statcast_search()] return value for the full column-by-column reference.
#' @export
#' @examples
#' \donttest{
#'   try({
#'     x <- statcast_search_pitchers(start_date = "2016-04-06",
#'       end_date = "2016-04-15", pitcherid = 592789)
#'     daily <- statcast_search_pitchers(start_date = "2016-04-06",
#'       end_date = "2016-04-06", pitcherid = NULL)
#'   })
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
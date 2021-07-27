#' Query Statcast and PITCHf/x Data for data from \url{http://baseballsavant.mlb.com}
#'
#' This function allows you to query Statcast and PITCHf/x data as provided on \url{http://baseballsavant.mlb.com} and have that data returned as a \code{\link{data.frame}}.
#' @param start_date Date of first game for which you want data.
#' Format must be in YYYY-MM-DD format.
#' @param end_date Date of last game for which you want data.
#' Format must be in YYYY-MM-DD format.
#' @param playerid The MLBAM ID for the player whose data you want to query.
#' @param player_type The player type. Can be \code{batter} or \code{pitcher}.
#' Default is \code{batter}
#' @param ... currently ignored
#' @keywords MLB, sabermetrics, Statcast
#' @importFrom tibble tribble
#' @importFrom lubridate year
#' @importFrom vroom vroom
#' @export
#' @examples
#' \dontrun{
#' correa <- scrape_statcast_savant(start_date = "2016-04-06",
#'   end_date = "2016-04-15", playerid = 621043)
#'
#' noah <- scrape_statcast_savant(start_date = "2016-04-06",
#'   end_date = "2016-04-15", playerid = 592789, player_type = 'pitcher')
#'
#' daily <- scrape_statcast_savant(start_date = "2016-04-06", end_date = "2016-04-06")
#' }

scrape_statcast_savant <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
                                   playerid = NULL,
                                   player_type = "batter", ...) UseMethod("scrape_statcast_savant")

#' @rdname scrape_statcast_savant
#' @export

scrape_statcast_savant.Date <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
                                        playerid = NULL, player_type = "batter", ...) {
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
    dplyr::mutate(pairs = paste0(var, "=", value))

  if (is.null(playerid)) {
    message("No playerid specified. Collecting data for all batters/pitchers.")
    vars <- vars %>% dplyr::filter(!grepl("lookup", var))
  }

  url_vars <- paste0(vars$pairs, collapse = "&")
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?", url_vars)
  message(url)

  # Do a try/catch to show errors that the user may encounter while downloading.
  tryCatch(
    {
      message("These data are from BaseballSavant and are property of MLB Advanced Media, L.P. All rights reserved.")
      message("Grabbing data, this may take a minute...")
      suppressMessages(
        suppressWarnings(
          # use vroom::vroom for significant speed improvment
          payload <- readr::read_csv(url)
        )
      )
    },
    error = function(cond) {
      message("URL does not seem to exist, please check your Internet connection")
      message("Original error message:")
      message(cond)
      stop("No payload acquired")
    },
    # this will never run??
    warning = function(cond) {
      message("Original warning message:")
      message(cond)
    }
  )
  # adjust to handle that vroom::vroom
  # returns 0 rows on failure but > 1 columns
  if (nrow(payload) > 1) {
    message("URL read and payload acquired successfully.")

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

    return(process_statcast_payload(payload))
  } else {
    warning("No valid data found")
    message("Make sure your playerid, player_type, and date range are correct")

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
    return(payload)
  }
}

#' @rdname scrape_statcast_savant
#' @export

scrape_statcast_savant.default <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
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

  scrape_statcast_savant(start_Date, end_Date,
                         playerid, player_type, ...)

}


#' @rdname scrape_statcast_savant
#' @param batterid The MLBAM ID for the batter whose data you want to query.
#' @export
#' @examples
#' \dontrun{
#' correa <- scrape_statcast_savant_batter(start_date = "2016-04-06",
#'   end_date = "2016-04-15", batterid = 621043)
#' }

scrape_statcast_savant_batter <- function(start_date, end_date, batterid, ...) {
  scrape_statcast_savant(start_date, end_date, playerid = batterid,
                         player_type = "batter", ...)
}

#' @rdname scrape_statcast_savant
#' @export
#' @examples
#' \dontrun{
#' daily <- scrape_statcast_savant_batter_all(start_date = "2016-04-06",
#'   end_date = "2016-04-06")
#' }

scrape_statcast_savant_batter_all <- function(start_date, end_date, ...) {
  scrape_statcast_savant(start_date, end_date, player_type = "batter", ...)
}

#' @rdname scrape_statcast_savant
#' @param pitcherid The MLBAM ID for the pitcher whose data you want to query.
#' @export
#' @examples
#' \dontrun{
#' noah <- scrape_statcast_savant_pitcher(start_date = "2016-04-06",
#'   end_date = "2016-04-15", pitcherid = 592789)
#' }

scrape_statcast_savant_pitcher <- function(start_date, end_date, pitcherid, ...) {
  scrape_statcast_savant(start_date, end_date, playerid = pitcherid,
                         player_type = "pitcher", ...)
}

#' @rdname scrape_statcast_savant
#' @export
#' @examples
#' \dontrun{
#' daily <- scrape_statcast_savant_pitcher_all(start_date = "2016-04-06",
#'   end_date = "2016-04-06")
#' }

scrape_statcast_savant_pitcher_all <- function(start_date, end_date, ...) {
  scrape_statcast_savant(start_date, end_date, player_type = "pitcher", ...)
}

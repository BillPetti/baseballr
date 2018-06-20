#' Query Statcast and PITCHf/x Data for data from baseballsavant.mlb.com
#'
#' This function allows you to query Statcast and PITCHf/x data as provided on baseballsavant.mlb.com and have that data returned as a dataframe.
#' @param start_date Date of first game for which you want data. Format must be in YYYY-MM-DD format.
#' @param end_date Date of last game for which you want data. Format must be in YYYY-MM-DD format.
#' @param playerid The MLBAM ID for the player whose data you want to query.
#' @param player_type The player type. Can be 'batter' or 'pitcher'. Default is 'batter'
#' @param ... currently ignored
#' @keywords MLB, sabermetrics, Statcast
#' @importFrom utils read.csv
#' @importFrom tibble tribble
#' @importFrom dplyr mutate_ filter_
#' @importFrom lubridate year
#' @export
#' @examples
#' \dontrun{
#' correa <- scrape_statcast_savant(start_date = "2016-04-06", 
#'   end_date = "2016-04-15", playerid = 621043)
#'
#' noah <- scrape_statcast_savant(start_date = "2016-04-06", 
#'   end_date = "2016-04-15", playerid = 592789, player_type='pitcher')
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
  if (start_date <= "2008-03-25") { # March 25, 2008 was the first date of Spring Training.
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
    "hfGT", "R%7C",
    "hfSea", paste0(lubridate::year(start_date), "%7C"),
    "player_type", player_type,
    playerid_var, ifelse(is.null(playerid), "", playerid),
    "game_date_gt", as.character(start_date),
    "game_date_lt", as.character(end_date),
    "min_pitches", 0,
    "min_results", 0,
    "group_by", "name",
    "sort_col", "pitches",
    "player_event_sort", "h_launch_speed",
    "sort_order", "desc",
    "min_abs", 0,
    "type", "details"
  ) %>%
    dplyr::mutate_(pairs = ~paste(var, "=", value, sep = ""))
  
  if (is.null(playerid)) {
    message("No playerid specified. Collecting data for all batters/pitchers.")
    vars <- dplyr::filter_(vars, ~!grepl("lookup", var))
  }
  
  url_vars <- paste0(vars$pairs, collapse = "&")
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?", url_vars)
  

  
  # Do a try/catch to show errors that the user may encounter while downloading.
  tryCatch(
    {
      message("These data are from BaseballSevant and are property of MLB Advanced Media, L.P. All rights reserved.")
      message("Grabbing data, this may take a minute...")
      payload <- utils::read.csv(url)
      processed_payload <- process_statcast_payload(payload)
      message("URL read and payload acquired successfully.")
      return(processed_payload)
    },
    error = function(cond) {
      message(paste("URL does not seem to exist, please check your Internet connection:"))
      message("Original error message:")
      message(cond)
      return(NA)
    },
    warning = function(cond) {
      message(paste("URL caused a warning. Make sure your playerid, player_type, and date range are correct:"))
      message("Original warning message:")
      message(cond)
      return(NULL)
    }
  )
  
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

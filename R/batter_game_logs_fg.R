#' Scrape Batter Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a batter from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @importFrom rlang .data
#' @importFrom dplyr mutate select 
#' @importFrom tidyr everything
#' @importFrom stringr str_extract
#' @importFrom jsonlite fromJSON
#' @import rvest
#' @export
#' @examples \donttest{
#'   batter_game_logs_fg(playerid = 6184, year = 2017)
#'  }

batter_game_logs_fg <- function(playerid, year = 2017) {

  fg_batter_game_logs(playerid = playerid, year = playerid)
  
}


#' Scrape Pitcher Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a pitcher from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @export
#' @examples \donttest{
#'   pitcher_game_logs_fg(playerid = 104, year = 2006)
#' }

pitcher_game_logs_fg <- function(playerid, year = 2017) {

  fg_pitcher_game_logs(playerid = playerid, year = year)
  
}

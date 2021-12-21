#' # Scrape MiLB game logs for pitchers from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MiLB game logs for individual batters from FanGraphs.com.
#' @param playerid The pitcher's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' @export
#' @examples \donttest{
#'   milb_pitcher_game_logs_fg(playerid = "sa3004210", year=2017)
#' }

milb_pitcher_game_logs_fg <- function(playerid, year) {
  
  fg_milb_pitcher_game_logs(playerid = playerid, year = year)
  
}

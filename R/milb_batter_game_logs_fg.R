#' # Scrape MiLB game logs for batters from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MiLB game logs for individual batters from FanGraphs.com.
#' @param playerid The batter's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' @export
#' @examples \donttest{
#'   milb_batter_game_logs_fg(playerid = "sa917940", year=2018)
#' }

milb_batter_game_logs_fg <- function(playerid, year = 2017) {
    
  fg_milb_batter_game_logs(playerid = playerid, year = year)
  
}

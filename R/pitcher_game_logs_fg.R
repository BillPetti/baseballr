#' Scrape Pitcher Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a pitcher from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @import rvest 
#' @export
#' @examples
#' \donttest{pitcher_game_logs_fg(playerid = 104, year = 2006)}

pitcher_game_logs_fg <- function(playerid, year = 2017) {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  url <- paste0("https://cdn.fangraphs.com/api/players/game-log?playerid=",
                playerid,
                "&position=P&type=0&gds=&gde=&z=1637143594112&season=",
                year)
  res <- httr::RETRY("GET", url)
  
  resp <- res %>% 
    httr::content(as = "text", encoding = "UTF-8")
  
  payload <- jsonlite::fromJSON(resp)[['mlb']] %>% 
    as.data.frame()
  payload <- payload[-1,]
  payload <- payload %>% 
    dplyr::mutate(
      Date = stringr::str_extract(.data$Date,"(?<=>).+(?=<)")) %>% 
    dplyr::select(.data$PlayerName, .data$playerid, tidyr::everything())
  return(payload)
}

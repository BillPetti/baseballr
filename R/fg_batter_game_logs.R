#' @rdname fg_batter_game_logs
#' @title Scrape Batter Game Logs from FanGraphs
#'
#' @description This function allows you to scrape game logs by year for a batter from FanGraphs.com.
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
#'   fg_batter_game_logs(playerid = 6184, year = 2017)
#'  }

fg_batter_game_logs <- function(playerid, year = 2017) {

  url <- paste0("https://cdn.fangraphs.com/api/players/game-log?playerid=",
                playerid,
                "&position=&type=0&gds=&gde=&z=1637143594112&season=",
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
#' @rdname fg_batter_game_logs
#' @title Scrape Batter Game Logs from FanGraphs
#' @description This function allows you to scrape game logs by year for a batter from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @export
batter_game_logs_fg <- function(playerid, year = 2017) {
  fg_batter_game_logs(playerid = playerid, year = year)
}


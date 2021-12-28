#' @rdname fg_pitcher_game_logs
#' @title **Scrape Pitcher Game Logs from FanGraphs**
#'
#' @description This function allows you to scrape game logs by year for a pitcher from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @return A data frame of pitcher game logs.
#' @importFrom dplyr mutate select 
#' @importFrom jsonlite fromJSON 
#' @importFrom stringr str_extract
#' @importFrom tidyr everything
#' @import rvest 
#' @export
#' @examples \donttest{
#'   fg_pitcher_game_logs(playerid = 104, year = 2006)
#' }

fg_pitcher_game_logs <- function(playerid, year = 2017) {

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


#' @rdname pitcher_game_logs_fg
#' @title **(legacy) Scrape Pitcher Game Logs from FanGraphs**
#' @inheritParams fg_pitcher_game_logs
#' @return A data frame of pitcher game logs.
#' @keywords legacy
#' @export
pitcher_game_logs_fg <- fg_pitcher_game_logs
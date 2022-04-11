#' @rdname fg_pitcher_game_logs
#' @title **Scrape Pitcher Game Logs from FanGraphs**
#'
#' @description This function allows you to scrape game logs by year for a pitcher from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @return A data frame of pitcher game logs.
#'  |col_name   |types     |
#'  |:----------|:---------|
#'  |PlayerName |character |
#'  |playerid   |integer   |
#'  |Date       |character |
#'  |Opp        |character |
#'  |teamid     |integer   |
#'  |season     |integer   |
#'  |Team       |character |
#'  |HomeAway   |character |
#'  |Age        |integer   |
#'  |W          |numeric   |
#'  |L          |numeric   |
#'  |ERA        |numeric   |
#'  |G          |numeric   |
#'  |GS         |numeric   |
#'  |CG         |numeric   |
#'  |ShO        |numeric   |
#'  |SV         |numeric   |
#'  |HLD        |numeric   |
#'  |BS         |numeric   |
#'  |IP         |numeric   |
#'  |TBF        |numeric   |
#'  |H          |numeric   |
#'  |R          |numeric   |
#'  |ER         |numeric   |
#'  |HR         |numeric   |
#'  |BB         |numeric   |
#'  |IBB        |numeric   |
#'  |HBP        |numeric   |
#'  |WP         |numeric   |
#'  |BK         |numeric   |
#'  |SO         |numeric   |
#'  |K/9        |numeric   |
#'  |BB/9       |numeric   |
#'  |H/9        |numeric   |
#'  |K/BB       |numeric   |
#'  |IFH%       |numeric   |
#'  |BUH%       |numeric   |
#'  |GB         |numeric   |
#'  |FB         |numeric   |
#'  |LD         |numeric   |
#'  |IFFB       |numeric   |
#'  |IFH        |numeric   |
#'  |BU         |numeric   |
#'  |BUH        |numeric   |
#'  |K%         |numeric   |
#'  |BB%        |numeric   |
#'  |K-BB%      |numeric   |
#'  |SIERA      |numeric   |
#'  |HR/9       |numeric   |
#'  |AVG        |numeric   |
#'  |WHIP       |numeric   |
#'  |BABIP      |numeric   |
#'  |LOB%       |numeric   |
#'  |FIP        |numeric   |
#'  |E-F        |numeric   |
#'  |xFIP       |numeric   |
#'  |ERA-       |numeric   |
#'  |FIP-       |numeric   |
#'  |xFIP-      |numeric   |
#'  |GB/FB      |numeric   |
#'  |LD%        |numeric   |
#'  |GB%        |numeric   |
#'  |FB%        |numeric   |
#'  |IFFB%      |numeric   |
#'  |HR/FB      |numeric   |
#'  |RS         |numeric   |
#'  |RS/9       |numeric   |
#'  |Balls      |numeric   |
#'  |Strikes    |numeric   |
#'  |Pitches    |numeric   |
#'  |WPA        |numeric   |
#'  |-WPA       |numeric   |
#'  |+WPA       |numeric   |
#'  |RE24       |numeric   |
#'  |REW        |numeric   |
#'  |pLI        |numeric   |
#'  |inLI       |numeric   |
#'  |gmLI       |numeric   |
#'  |exLI       |numeric   |
#'  |Pulls      |numeric   |
#'  |Games      |numeric   |
#'  |WPA/LI     |numeric   |
#'  |Clutch     |numeric   |
#'  |SD         |numeric   |
#'  |MD         |numeric   |
#'  |FB%1       |numeric   |
#'  |FBv        |numeric   |
#'  |SL%        |numeric   |
#'  |SLv        |numeric   |
#'  |CT%        |numeric   |
#'  |CTv        |numeric   |
#'  |CB%        |numeric   |
#'  |CBv        |numeric   |
#'  |CH%        |numeric   |
#'  |CHv        |numeric   |
#'  |XX%        |numeric   |
#'  |PO%        |numeric   |
#'  |wFB        |numeric   |
#'  |wSL        |numeric   |
#'  |wCT        |numeric   |
#'  |wCB        |numeric   |
#'  |wCH        |numeric   |
#'  |wFB/C      |numeric   |
#'  |wSL/C      |numeric   |
#'  |wCT/C      |numeric   |
#'  |wCB/C      |numeric   |
#'  |wCH/C      |numeric   |
#'  |O-Swing%   |numeric   |
#'  |Z-Swing%   |numeric   |
#'  |Swing%     |numeric   |
#'  |O-Contact% |numeric   |
#'  |Z-Contact% |numeric   |
#'  |Contact%   |numeric   |
#'  |Zone%      |numeric   |
#'  |F-Strike%  |numeric   |
#'  |SwStr%     |numeric   |
#'  |Pull       |numeric   |
#'  |Cent       |numeric   |
#'  |Oppo       |numeric   |
#'  |Soft       |numeric   |
#'  |Med        |numeric   |
#'  |Hard       |numeric   |
#'  |bipCount   |numeric   |
#'  |Pull%      |numeric   |
#'  |Cent%      |numeric   |
#'  |Oppo%      |numeric   |
#'  |Soft%      |numeric   |
#'  |Med%       |numeric   |
#'  |Hard%      |numeric   |
#'  |tERA       |numeric   |
#'  |GSv2       |numeric   |
#'  |Events     |numeric   |
#'  |gamedate   |character |
#'  |dh         |integer   |
#' @importFrom dplyr mutate select 
#' @importFrom jsonlite fromJSON 
#' @importFrom stringr str_extract
#' @importFrom tidyr everything
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(fg_pitcher_game_logs(playerid = 104, year = 2006))
#' }

fg_pitcher_game_logs <- function(playerid, year = 2017) {
  
  url <- paste0("https://cdn.fangraphs.com/api/players/game-log?playerid=",
                playerid,
                "&position=P&type=0&gds=&gde=&z=1637143594112&season=",
                year)
  
  tryCatch(
    expr={
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
      payload <- payload %>%
        make_baseballr_data("MLB Pitcher Game Log data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no pitcher game log data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(payload)
}  


#' @rdname pitcher_game_logs_fg
#' @title **(legacy) Scrape Pitcher Game Logs from FanGraphs**
#' @inheritParams fg_pitcher_game_logs
#' @return A data frame of pitcher game logs.
#' @keywords legacy
#' @export
pitcher_game_logs_fg <- fg_pitcher_game_logs
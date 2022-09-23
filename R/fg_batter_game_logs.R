#' @rdname fg_batter_game_logs
#' @title **Scrape Batter Game Logs from FanGraphs**
#' @description This function allows you to scrape game logs by year for a batter from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @return A data frame of batter game logs.
#'     |col_name      |types     |
#'     |:-------------|:---------|
#'     |PlayerName    |character |
#'     |playerid      |integer   |
#'     |Date          |character |
#'     |Team          |character |
#'     |Opp           |character |
#'     |season        |integer   |
#'     |Age           |integer   |
#'     |BatOrder      |character |
#'     |Pos           |character |
#'     |G             |numeric   |
#'     |AB            |numeric   |
#'     |PA            |numeric   |
#'     |H             |numeric   |
#'     |1B            |numeric   |
#'     |2B            |numeric   |
#'     |3B            |numeric   |
#'     |HR            |numeric   |
#'     |R             |numeric   |
#'     |RBI           |numeric   |
#'     |BB            |numeric   |
#'     |IBB           |numeric   |
#'     |SO            |numeric   |
#'     |HBP           |numeric   |
#'     |SF            |numeric   |
#'     |SH            |numeric   |
#'     |GDP           |numeric   |
#'     |SB            |numeric   |
#'     |CS            |numeric   |
#'     |AVG           |numeric   |
#'     |GB            |numeric   |
#'     |FB            |numeric   |
#'     |LD            |numeric   |
#'     |IFFB          |numeric   |
#'     |Pitches       |numeric   |
#'     |Balls         |numeric   |
#'     |Strikes       |numeric   |
#'     |IFH           |numeric   |
#'     |BU            |numeric   |
#'     |BUH           |numeric   |
#'     |BB%           |numeric   |
#'     |K%            |numeric   |
#'     |BB/K          |numeric   |
#'     |OBP           |numeric   |
#'     |SLG           |numeric   |
#'     |OPS           |numeric   |
#'     |ISO           |numeric   |
#'     |BABIP         |numeric   |
#'     |GB/FB         |numeric   |
#'     |LD%           |numeric   |
#'     |GB%           |numeric   |
#'     |FB%           |numeric   |
#'     |IFFB%         |numeric   |
#'     |HR/FB         |numeric   |
#'     |IFH%          |numeric   |
#'     |BUH%          |numeric   |
#'     |wOBA          |numeric   |
#'     |wRAA          |numeric   |
#'     |wRC           |numeric   |
#'     |Spd           |numeric   |
#'     |wRC+          |numeric   |
#'     |wBSR          |numeric   |
#'     |WPA           |numeric   |
#'     |-WPA          |numeric   |
#'     |+WPA          |numeric   |
#'     |RE24          |numeric   |
#'     |REW           |numeric   |
#'     |pLI           |numeric   |
#'     |phLI          |numeric   |
#'     |PH            |numeric   |
#'     |WPA/LI        |numeric   |
#'     |Clutch        |numeric   |
#'     |FB%1          |numeric   |
#'     |FBv           |numeric   |
#'     |SL%           |numeric   |
#'     |SLv           |numeric   |
#'     |CT%           |numeric   |
#'     |CTv           |numeric   |
#'     |CB%           |numeric   |
#'     |CBv           |numeric   |
#'     |CH%           |numeric   |
#'     |CHv           |numeric   |
#'     |SF%           |numeric   |
#'     |SFv           |numeric   |
#'     |KN%           |numeric   |
#'     |KNv           |numeric   |
#'     |XX%           |numeric   |
#'     |wFB           |numeric   |
#'     |wSL           |numeric   |
#'     |wCT           |numeric   |
#'     |wCB           |numeric   |
#'     |wCH           |numeric   |
#'     |wSF           |numeric   |
#'     |wKN           |numeric   |
#'     |wFB/C         |numeric   |
#'     |wSL/C         |numeric   |
#'     |wCT/C         |numeric   |
#'     |wCB/C         |numeric   |
#'     |wCH/C         |numeric   |
#'     |wSF/C         |numeric   |
#'     |wKN/C         |numeric   |
#'     |O-Swing%      |numeric   |
#'     |Z-Swing%      |numeric   |
#'     |Swing%        |numeric   |
#'     |O-Contact%    |numeric   |
#'     |Z-Contact%    |numeric   |
#'     |Contact%      |numeric   |
#'     |Zone%         |numeric   |
#'     |F-Strike%     |numeric   |
#'     |SwStr%        |numeric   |
#'     |Pull          |numeric   |
#'     |Cent          |numeric   |
#'     |Oppo          |numeric   |
#'     |Soft          |numeric   |
#'     |Med           |numeric   |
#'     |Hard          |numeric   |
#'     |bipCount      |numeric   |
#'     |Pull%         |numeric   |
#'     |Cent%         |numeric   |
#'     |Oppo%         |numeric   |
#'     |Soft%         |numeric   |
#'     |Med%          |numeric   |
#'     |Hard%         |numeric   |
#'     |pfxFA%        |numeric   |
#'     |pfxFT%        |numeric   |
#'     |pfxFC%        |numeric   |
#'     |pfxFS%        |numeric   |
#'     |pfxFO%        |numeric   |
#'     |pfxSI%        |numeric   |
#'     |pfxSL%        |numeric   |
#'     |pfxCU%        |numeric   |
#'     |pfxKC%        |numeric   |
#'     |pfxCH%        |numeric   |
#'     |pfxKN%        |numeric   |
#'     |pfxvFA        |numeric   |
#'     |pfxvFT        |numeric   |
#'     |pfxvFC        |numeric   |
#'     |pfxvFS        |numeric   |
#'     |pfxvFO        |numeric   |
#'     |pfxvSI        |numeric   |
#'     |pfxvSL        |numeric   |
#'     |pfxvCU        |numeric   |
#'     |pfxvKC        |numeric   |
#'     |pfxvCH        |numeric   |
#'     |pfxvKN        |numeric   |
#'     |pfxFA-X       |numeric   |
#'     |pfxFT-X       |numeric   |
#'     |pfxFC-X       |numeric   |
#'     |pfxFS-X       |numeric   |
#'     |pfxFO-X       |numeric   |
#'     |pfxSI-X       |numeric   |
#'     |pfxSL-X       |numeric   |
#'     |pfxCU-X       |numeric   |
#'     |pfxKC-X       |numeric   |
#'     |pfxCH-X       |numeric   |
#'     |pfxKN-X       |numeric   |
#'     |pfxFA-Z       |numeric   |
#'     |pfxFT-Z       |numeric   |
#'     |pfxFC-Z       |numeric   |
#'     |pfxFS-Z       |numeric   |
#'     |pfxFO-Z       |numeric   |
#'     |pfxSI-Z       |numeric   |
#'     |pfxSL-Z       |numeric   |
#'     |pfxCU-Z       |numeric   |
#'     |pfxKC-Z       |numeric   |
#'     |pfxCH-Z       |numeric   |
#'     |pfxKN-Z       |numeric   |
#'     |pfxwFA        |numeric   |
#'     |pfxwFT        |numeric   |
#'     |pfxwFC        |numeric   |
#'     |pfxwFS        |numeric   |
#'     |pfxwFO        |numeric   |
#'     |pfxwSI        |numeric   |
#'     |pfxwSL        |numeric   |
#'     |pfxwCU        |numeric   |
#'     |pfxwKC        |numeric   |
#'     |pfxwCH        |numeric   |
#'     |pfxwKN        |numeric   |
#'     |pfxwFA/C      |numeric   |
#'     |pfxwFT/C      |numeric   |
#'     |pfxwFC/C      |numeric   |
#'     |pfxwFS/C      |numeric   |
#'     |pfxwFO/C      |numeric   |
#'     |pfxwSI/C      |numeric   |
#'     |pfxwSL/C      |numeric   |
#'     |pfxwCU/C      |numeric   |
#'     |pfxwKC/C      |numeric   |
#'     |pfxwCH/C      |numeric   |
#'     |pfxwKN/C      |numeric   |
#'     |pfxO-Swing%   |numeric   |
#'     |pfxZ-Swing%   |numeric   |
#'     |pfxSwing%     |numeric   |
#'     |pfxO-Contact% |numeric   |
#'     |pfxZ-Contact% |numeric   |
#'     |pfxContact%   |numeric   |
#'     |pfxZone%      |numeric   |
#'     |pfxPace       |numeric   |
#'     |piCH%         |numeric   |
#'     |piCS%         |numeric   |
#'     |piCU%         |numeric   |
#'     |piFA%         |numeric   |
#'     |piFC%         |numeric   |
#'     |piFS%         |numeric   |
#'     |piKN%         |numeric   |
#'     |piSI%         |numeric   |
#'     |piSL%         |numeric   |
#'     |piXX%         |numeric   |
#'     |pivCH         |numeric   |
#'     |pivCS         |numeric   |
#'     |pivCU         |numeric   |
#'     |pivFA         |numeric   |
#'     |pivFC         |numeric   |
#'     |pivFS         |numeric   |
#'     |pivKN         |numeric   |
#'     |pivSI         |numeric   |
#'     |pivSL         |numeric   |
#'     |pivXX         |numeric   |
#'     |piCH-X        |numeric   |
#'     |piCS-X        |numeric   |
#'     |piCU-X        |numeric   |
#'     |piFA-X        |numeric   |
#'     |piFC-X        |numeric   |
#'     |piFS-X        |numeric   |
#'     |piKN-X        |numeric   |
#'     |piSI-X        |numeric   |
#'     |piSL-X        |numeric   |
#'     |piXX-X        |numeric   |
#'     |piCH-Z        |numeric   |
#'     |piCS-Z        |numeric   |
#'     |piCU-Z        |numeric   |
#'     |piFA-Z        |numeric   |
#'     |piFC-Z        |numeric   |
#'     |piFS-Z        |numeric   |
#'     |piKN-Z        |numeric   |
#'     |piSI-Z        |numeric   |
#'     |piSL-Z        |numeric   |
#'     |piXX-Z        |numeric   |
#'     |piwCH         |numeric   |
#'     |piwCS         |numeric   |
#'     |piwCU         |numeric   |
#'     |piwFA         |numeric   |
#'     |piwFC         |numeric   |
#'     |piwFS         |numeric   |
#'     |piwKN         |numeric   |
#'     |piwSI         |numeric   |
#'     |piwSL         |numeric   |
#'     |piwXX         |numeric   |
#'     |piwCH/C       |numeric   |
#'     |piwCS/C       |numeric   |
#'     |piwCU/C       |numeric   |
#'     |piwFA/C       |numeric   |
#'     |piwFC/C       |numeric   |
#'     |piwFS/C       |numeric   |
#'     |piwKN/C       |numeric   |
#'     |piwSI/C       |numeric   |
#'     |piwSL/C       |numeric   |
#'     |piwXX/C       |numeric   |
#'     |piO-Swing%    |numeric   |
#'     |piZ-Swing%    |numeric   |
#'     |piSwing%      |numeric   |
#'     |piO-Contact%  |numeric   |
#'     |piZ-Contact%  |numeric   |
#'     |piContact%    |numeric   |
#'     |piZone%       |numeric   |
#'     |Events        |numeric   |
#'     |EV            |numeric   |
#'     |LA            |numeric   |
#'     |Barrels       |numeric   |
#'     |Barrel%       |numeric   |
#'     |maxEV         |numeric   |
#'     |HardHit       |numeric   |
#'     |HardHit%      |numeric   |
#'     |gamedate      |character |
#'     |dh            |integer   |
#' @importFrom rlang .data
#' @importFrom dplyr mutate select 
#' @importFrom tidyr everything
#' @importFrom stringr str_extract
#' @importFrom jsonlite fromJSON
#' @import rvest
#' @export
#' @examples \donttest{
#'   try(fg_batter_game_logs(playerid = 6184, year = 2017))
#' }

fg_batter_game_logs <- function(playerid, year = 2017) {
  
  url <- paste0("https://cdn.fangraphs.com/api/players/game-log?playerid=",
                playerid,
                "&position=&type=0&gds=&gde=&z=1637143594112&season=",
                year)
  tryCatch(
    expr = {
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
        make_baseballr_data("MLB Batter Game Logs data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no batter game logs data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(payload)
}

#' @rdname batter_game_logs_fg
#' @title **(legacy) Scrape Batter Game Logs from FanGraphs**
#' @inheritParams fg_batter_game_logs
#' @return A data frame of batter game logs.
#' @keywords legacy
#' @export
batter_game_logs_fg <- fg_batter_game_logs
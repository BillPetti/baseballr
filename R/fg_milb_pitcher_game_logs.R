#' @rdname fg_milb_pitcher_game_logs
#' @title **Scrape MiLB game logs for pitchers from FanGraphs**
#'
#' @description This function allows you to scrape MiLB game logs for individual batters from FanGraphs.com.
#' @param playerid The pitcher's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' @return Returns a data frame of Minor League pitcher game logs.
#'  |col_name       |types     |
#'  |:--------------|:---------|
#'  |player_name    |character |
#'  |minor_playerid |character |
#'  |Date           |character |
#'  |Team           |character |
#'  |Level          |character |
#'  |Opp            |character |
#'  |W              |numeric   |
#'  |L              |numeric   |
#'  |ERA            |numeric   |
#'  |G              |numeric   |
#'  |GS             |numeric   |
#'  |CG             |numeric   |
#'  |ShO            |numeric   |
#'  |SV             |numeric   |
#'  |IP             |numeric   |
#'  |TBF            |numeric   |
#'  |H              |numeric   |
#'  |R              |numeric   |
#'  |ER             |numeric   |
#'  |HR             |numeric   |
#'  |BB             |numeric   |
#'  |IBB            |numeric   |
#'  |HBP            |numeric   |
#'  |WP             |numeric   |
#'  |BK             |numeric   |
#'  |SO             |numeric   |
#'  |K/9            |numeric   |
#'  |BB/9           |numeric   |
#'  |K/BB           |numeric   |
#'  |HR/9           |numeric   |
#'  |K%             |numeric   |
#'  |K-BB%          |numeric   |
#'  |BB%            |numeric   |
#'  |AVG            |numeric   |
#'  |WHIP           |numeric   |
#'  |BABIP          |numeric   |
#'  |LOB%           |numeric   |
#'  |FIP            |numeric   |
#'  |gamedate       |character |
#'  |dh             |integer   |
#' @import rvest 
#' @importFrom tidyr separate
#' @export
#' @examples \donttest{
#'   fg_milb_pitcher_game_logs(playerid = "sa3004210", year=2017)
#' }

fg_milb_pitcher_game_logs <- function(playerid, year) {
  
  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                      playerid,
                      "&season=",
                      year,
                      "&position=P","&type=-1")
  # CDN API game-log
  url <- paste0("https://cdn.fangraphs.com/api/players/game-log?position=P&type=-1&&gds=&gde=&z=1637230004112&playerid=",
                playerid,
                "&season=",
                year)
  
  res <- httr::RETRY("GET", url)
  
  resp <- res %>% 
    httr::content(as = "text", encoding = "UTF-8")
  
  payload <- jsonlite::fromJSON(resp)[['minor']] %>% 
    as.data.frame()
  # remove averages/totals column
  payload <- payload[-1,]
  # separate Team column into Team & MiLB level
  suppressWarnings(
    payload <- payload %>%
      tidyr::separate(.data$Team, into = c("Team","Level"),sep=" ")
  )
  # extract player name
  player_name <- url_basic %>% 
    xml2::read_html() %>%
    rvest::html_elements("h1") %>%
    rvest::html_text()
  
  # add playerid to payload
  payload <- payload %>%
    dplyr::mutate(
      player_name = player_name,
      minor_playerid = playerid) %>%
    dplyr::select(.data$player_name, .data$minor_playerid, tidyr::everything())
  
  return(payload)
}

#' @rdname milb_pitcher_game_logs_fg
#' @title **(legacy) Scrape MiLB game logs for pitchers from FanGraphs**
#' @inheritParams fg_milb_pitcher_game_logs
#' @return Returns a data frame of Minor League pitcher game logs.
#' @keywords legacy
#' @export
milb_pitcher_game_logs_fg <-  fg_milb_pitcher_game_logs
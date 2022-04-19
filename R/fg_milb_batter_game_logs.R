#' @rdname fg_milb_batter_game_logs
#' @title **Scrape MiLB game logs for batters from FanGraphs**
#' @description This function allows you to scrape MiLB game logs for individual batters from FanGraphs.
#' @param playerid The batter's minor league ID from FanGraphs.
#' @param year The season for which game logs should be returned.
#' @return Returns a tibble of Minor League batter game logs with the following columns:
#' 
#'  |col_name       |types     |
#'  |:--------------|:---------|
#'  |player_name    |character |
#'  |minor_playerid |character |
#'  |Date           |character |
#'  |Team           |character |
#'  |Level          |character |
#'  |Opp            |character |
#'  |G              |numeric   |
#'  |AB             |numeric   |
#'  |PA             |numeric   |
#'  |H              |numeric   |
#'  |1B             |numeric   |
#'  |2B             |numeric   |
#'  |3B             |numeric   |
#'  |HR             |numeric   |
#'  |R              |numeric   |
#'  |RBI            |numeric   |
#'  |BB             |numeric   |
#'  |IBB            |numeric   |
#'  |SO             |numeric   |
#'  |HBP            |numeric   |
#'  |SF             |numeric   |
#'  |SH             |numeric   |
#'  |GDP            |numeric   |
#'  |SB             |numeric   |
#'  |CS             |numeric   |
#'  |AVG            |numeric   |
#'  |BB%            |numeric   |
#'  |K%             |numeric   |
#'  |BB/K           |numeric   |
#'  |OBP            |numeric   |
#'  |SLG            |numeric   |
#'  |OPS            |numeric   |
#'  |ISO            |numeric   |
#'  |Spd            |numeric   |
#'  |BABIP          |numeric   |
#'  |wRC            |numeric   |
#'  |wRAA           |numeric   |
#'  |wOBA           |numeric   |
#'  |wRC+           |numeric   |
#'  |wBsR           |numeric   |
#'  |gamedate       |character |
#'  |dh             |integer   |
#'  
#' @importFrom tidyr separate everything
#' @importFrom dplyr mutate select
#' @importFrom jsonlite fromJSON
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(fg_milb_batter_game_logs(playerid = "sa3010868", year=2021))
#' }

fg_milb_batter_game_logs <- function(playerid, year) {
  tryCatch(
    expr={
      
      # url for standard game log table
      url_basic <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                          playerid,
                          "&season=",
                          year,
                          "&position=PB","&type=-1")
      # CDN API game-log
      url <- paste0("https://cdn.fangraphs.com/api/players/game-log?position=&type=-1&&gds=&gde=&z=1637230004112&playerid=",
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
      
      payload <- payload %>%
        make_baseballr_data("MiLB Batter Game Logs data from FanGraphs.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no MiLB batter game logs data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(payload)
}  


#' @rdname milb_batter_game_logs_fg
#' @title **(legacy) Scrape MiLB game logs for batters from FanGraphs**
#' @inheritParams fg_milb_batter_game_logs
#' @return Returns a tibble of Minor League batter game logs.
#' @keywords legacy
#' @export

milb_batter_game_logs_fg <- fg_milb_batter_game_logs

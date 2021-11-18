#' # Scrape MiLB game logs for batters from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MiLB game logs for individual batters from FanGraphs.com.
#' @param playerid The batter's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' @import rvest 
#' @importFrom tidyr separate
#' @importFrom readr parse_number
#' @export
#' @examples
#' \donttest{milb_batter_game_logs_fg(playerid = "sa917940", year=2018)}

milb_batter_game_logs_fg <- function(playerid, year = 2017) {
    
    message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')
    
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
        separate(.data$Team, into = c("Team","Level"),sep=" ")
    )
    # extract player name
    player_name <- xml2::read_html(url_basic) %>%
      rvest::html_nodes("h1") %>%
      rvest::html_text()
    
    # add playerid to payload
    payload <- payload %>%
      dplyr::mutate(
        player_name = player_name,
        minor_playerid = playerid) %>%
      dplyr::select(.data$player_name, .data$minor_playerid, tidyr::everything())
    
    return(payload)
}

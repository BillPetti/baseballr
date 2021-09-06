#' # Scrape MiLB game logs for pitchers from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MiLB game logs for individual batters from FanGraphs.com.
#' @param playerid The pitcher's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_attr
#' @importFrom tidyr separate
#' @importFrom readr parse_number
#' @export
#' @examples
#' \dontrun{milb_pitcher_game_logs_fg(playerid = "sa3004210", year=2021)}

milb_pitcher_game_logs_fg <- function(playerid, year = 2017) {
  
  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')
  
  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                      playerid,
                      "&season=",
                      year,
                      "&position=P","&type=-1")
  
  # url for advanced game log table
  url_adv <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                    playerid,
                    "&season=",
                    year,
                    "&position=P","&type=-2")
  
  # standard table
  payload1 <- (xml2::read_html(url_basic) %>%
                 rvest::html_nodes("table#DailyStats1_dgSeason1_ctl00")) %>%
    rvest::html_table() %>%
    as.data.frame()
  
  payload1 <- payload1 %>%
    dplyr::filter(!grepl("Date|Total", .data$Date)) 
  
  # advanced table
  payload2 <- (xml2::read_html(url_adv) %>%
                 rvest::html_nodes("table#DailyStats1_dgSeason1_ctl00")) %>%
    rvest::html_table() %>%
    as.data.frame()
  
  payload2 <- payload2 %>%
    dplyr::filter(!grepl("Date|Total", .data$Date)) 
  suppressWarnings(
    payload2 <- as.data.frame(sapply(payload2, function(x) (as.numeric(gsub("[\\%,]", "", x)))),
                              stringsAsFactors=F)
  )
  payload2 <- payload2 %>% 
    dplyr::rename(
      BB_perc = .data$BB., 
      K_perc = .data$K.,
      K_minus_BB = .data$K.BB., 
      LOB_perc = .data$LOB.)
  payload2$BB_perc <- as.numeric(payload2$BB_perc)/100
  payload2$K_perc <- as.numeric(payload2$K_perc)/100
  payload2$K_minus_BB <- as.numeric(payload2$K_minus_BB)/100
  payload2$LOB_perc <- as.numeric(payload2$LOB_perc)/100
  
  # combine standard & advanced game log tables
  payload <- dplyr::bind_cols(payload1 %>% dplyr::select(-.data$ERA),
                              payload2 %>% 
                                dplyr::select(-.data$Date,-.data$Team,-.data$Opp, -.data$GS))
  
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
      name = player_name,
      minor_playerid = playerid) %>%
    dplyr::select(.data$name, .data$minor_playerid, tidyr::everything())
  numeric_cols <- c("GS", "W", "L", "CG", "ShO", "SV", "IP", 
                    "TBF", "H", "R", "ER", "HR", "BB", "IBB", 
                    "HBP", "WP", "BK", "SO", "K.9", "BB.9", "K.BB", 
                    "HR.9", "K_perc", "BB_perc", "K_minus_BB", 
                    "AVG", "WHIP", "BABIP", "LOB_perc", "ERA", "FIP")
  
  suppressWarnings(
    payload <- payload %>% 
      dplyr::mutate_at(numeric_cols,as.numeric)
  )
  return(payload)
}

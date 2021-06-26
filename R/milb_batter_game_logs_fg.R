#' # Scrape MiLB game logs for batters from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MILB game logs for individual batters from FanGraphs.com.
#' @param playerid The batter's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' 
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_attr
#' @importFrom tidyr separate
#' @export
#' @examples
#' \dontrun{milb_batter_game_logs_fg(playerid = "sa917940", year=2018)}

milb_batter_game_logs_fg <- function(playerid, year = 2017) {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                      playerid,
                      "&season=",
                      year,
                      "&position=PB","&type=-1")

  # url for advanced game log table
  url_adv <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                    playerid,
                    "&season=",
                    year,
                    "&position=PB","&type=-2")

  # standard table
  payload1 <- (xml2::read_html(url_basic) %>%
    rvest::html_nodes("table"))[15] %>%
    rvest::html_table() %>%
    as.data.frame()

  payload1 <- payload1 %>%
    dplyr::filter(!grepl("Total|Postseason", .data$Season)) 
  
  # advanced table
  payload2 <- (xml2::read_html(url_adv) %>%
    rvest::html_elements("table"))[16] %>%
    rvest::html_table() %>%
    as.data.frame()

  payload2 <- payload2 %>%
    dplyr::filter(!grepl("Total|Postseason", .data$Season),
                  !grepl("Average", .data$Team)) 

  payload2 <- as.data.frame(sapply(payload2, function(x) (as.numeric(gsub("[\\%,]", "", x)))),
                           stringsAsFactors=F)
  
  payload2 <- payload2 %>% 
    dplyr::rename(
      BB_perc = .data$BB., 
      K_perc = .data$K.,
      BB_minus_K = .data$BB.K)

  # combine standard & advanced game log tabs
  payload <- dplyr::bind_cols(payload1,payload2 %>% 
                                dplyr::select(-.data$Season,-.data$Team,-.data$AVG))
  
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

  return(payload)
}

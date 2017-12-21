#' Scrape Batter Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a batter from from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @keywords MLB, sabermetrics
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom dplyr filter rename
#' @export
#' @examples
#' \dontrun{batter_game_logs_fg(playerid = 6184, year = 2017)}

batter_game_logs_fg <- function(playerid, year = 2017) {
  url <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                playerid,
                "&position=&season=",
                year)

  payload <- xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    .[14] %>%
    rvest::html_table() %>%
    as.data.frame()

  payload <- payload %>%
    dplyr::filter(!grepl("Date|Total", Date)) %>%
    dplyr::rename(BB_perc = BB., K_perc = K.,
           wRC_plus = wRC.)

  payload <- as.data.frame(sapply(payload, function(x) (gsub("\\ %", "", x))),
                              stringsAsFactors=F)

  payload$BB_perc <- as.numeric(payload$BB_perc)/100
  payload$K_perc <- as.numeric(payload$K_perc)/100

  payload
}

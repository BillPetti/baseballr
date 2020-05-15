#' Scrape Pitcher Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a pitcher from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @keywords MLB, sabermetrics
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @export
#' @examples
#' \dontrun{pitcher_game_logs_fg(playerid = 104, year = 2006)}

pitcher_game_logs_fg <- function(playerid, year = 2017) {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  url <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                playerid,
                "&season=",
                year, "&position=P")

  payload <- xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table(fill = TRUE) %>%
    as.data.frame()

  payload <- payload %>%
    dplyr::filter(!grepl("Date|Total", Date)) %>%
    dplyr::rename(K_9 = K.9, BB_9 = BB.9, HR_9 = HR.9,
                  LOB_perc = LOB., GB_perc = GB.,
                  HR_FB = HR.FB)

  if (nrow(payload) > 1) {

    payload <- as.data.frame(sapply(payload, function(x) (gsub("\\ %", "", x))),
                             stringsAsFactors=F)
  } else {

    payload <- lapply(payload, function(x) (gsub("\\ %",
                                                 "", x))) %>%
      bind_rows()

  }

  payload$K_9 <- as.numeric(payload$K_9)
  payload$BB_9 <- as.numeric(payload$BB_9)
  payload$HR_9 <- as.numeric(payload$HR_9)
  payload$LOB_perc <- as.numeric(payload$LOB_perc)/100
  payload$GB_perc <- as.numeric(payload$GB_perc)/100
  payload$HR_FB <- as.numeric(payload$HR_FB)

  payload
}

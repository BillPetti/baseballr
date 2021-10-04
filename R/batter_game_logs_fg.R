#' Scrape Batter Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a batter from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @importFrom rlang .data
#' @importFrom rvest html_nodes html_table
#' @export
#' @examples
#' \dontrun{batter_game_logs_fg(playerid = 6184, year = 2017)}

batter_game_logs_fg <- function(playerid, year = 2017) {

  message('Data courtey of FanGraphs. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  url <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                playerid,
                "&season=",
                year,
                "&position=PB")

  payload <- (xml2::read_html(url) %>%
    rvest::html_elements("table"))[16] %>%
    rvest::html_table(fill = TRUE) %>%
    as.data.frame()

  payload <- payload %>%
    dplyr::filter(!grepl("Date|Total", .data$Date)) 


  if (nrow(payload) > 1) {
    suppressWarnings(
      payload <- as.data.frame(sapply(payload, function(x) (as.numeric(gsub("[\\%,]", "", x)))),
                               stringsAsFactors=F)
    )
  } else {
    suppressWarnings(
      payload <- lapply(payload, function(x) (as.numeric(gsub("[\\%,]", "", x)))) %>%
        dplyr::bind_rows()
    )
  }
  payload <- payload %>% 
    dplyr::rename(
      BB_perc = .data$BB., 
      K_perc = .data$K.,
      wRC_plus = .data$wRC.)
  payload$BB_perc <- as.numeric(payload$BB_perc)/100
  payload$K_perc <- as.numeric(payload$K_perc)/100

  return(payload)
}


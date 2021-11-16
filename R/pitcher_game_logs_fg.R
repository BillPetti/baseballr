#' Scrape Pitcher Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a pitcher from FanGraphs.com.
#' @param playerid This is the playerid used by FanGraphs for a given player
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @export
#' @examples
#' \donttest{pitcher_game_logs_fg(playerid = 104, year = 2006)}

pitcher_game_logs_fg <- function(playerid, year = 2017) {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  url <- paste0("http://www.fangraphs.com/statsd-legacy.aspx?playerid=",
                playerid,
                "&season=",
                year, "&position=P")

  payload <- (xml2::read_html(url) %>%
    rvest::html_nodes("table"))[16] %>%
    rvest::html_table(fill = TRUE) %>%
    as.data.frame()

  payload <- payload %>%
    dplyr::filter(!grepl("Date|Total", .data$Date)) 
  if (nrow(payload) > 1) {
    suppressWarnings(
      payload <- payload %>% 
        dplyr::mutate(
          K.9 = as.numeric(.data$K.9),
          BB.9 = as.numeric(.data$BB.9),
          HR.9 = as.numeric(.data$HR.9),
          LOB. = as.numeric(gsub("[\\%,]", "", .data$LOB.)),
          GB. = as.numeric(gsub("[\\%,]", "", .data$GB.)),
          HR.FB = as.numeric(gsub("[\\%,]", "", .data$HR.FB))) %>% 
        dplyr::rename(
          K_9 = .data$K.9,
          BB_9 = .data$BB.9,
          HR_9 = .data$HR.9,
          LOB_perc = .data$LOB., 
          GB_perc = .data$GB.,
          HR_FB = .data$HR.FB)
    )
  } else {
    suppressWarnings(
      payload <- lapply(payload, function(x) (as.numeric(gsub("[\\%,]", "", x)))) %>%
        dplyr::bind_rows()
    )
  }
  
 
  payload$LOB_perc <- as.numeric(payload$LOB_perc)/100
  payload$GB_perc <- as.numeric(payload$GB_perc)/100

  return(payload)
}

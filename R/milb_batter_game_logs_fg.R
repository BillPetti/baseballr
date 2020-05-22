#' # Scrape MiLB game logs for batters from Fangraphs, combining 'standard' and 'advanced' tabs
#'
#' This function allows you to scrape MILB game logs for individual batters from FanGraphs.com.
#' @param playerid The batter's minor leauge ID from FanGraphs.com.
#' @param year The season for which game logs should be returned.
#' @keywords MLB, sabermetrics
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_attr
#' @importFrom tidyr separate
#' @export
#' @examples
#' \dontrun{milb_batter_game_logs_fg(playerid = "sa917940", 2018)}

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
  payload1 <- xml2::read_html(url_basic) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()

  payload1 <- payload1 %>%
    dplyr::filter(!grepl("Date|Total", Date))

  # advanced table
  payload2 <- xml2::read_html(url_adv) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()

  payload2 <- payload2 %>%
    dplyr::filter(!grepl("Date|Total", Date)) %>%
    dplyr::rename(BB_perc = BB., K_perc = K.,
                wRC_plus = wRC., BB_per_K = BB.K)

  payload2 <- as.data.frame(sapply(payload2, function(x) (gsub("\\ %", "", x))),
                           stringsAsFactors=F)

  payload2$BB_perc <- as.numeric(payload2$BB_perc)/100
  payload2$K_perc <- as.numeric(payload2$K_perc)/100

  # combine standard & advanced game log tabs
  payload <- merge(payload1,payload2)

  # separate Team column into Team & MiLB level
  payload <- payload %>%
    separate(Team, into = c("Team","Level"),sep=" ")

  # extract player name

  player_name <- xml2::read_html(url_basic) %>%
    rvest::html_nodes("h1") %>%
    rvest::html_text()

  # add playerid to payload

  payload <- payload %>%
    mutate(name = player_name,
           minor_playerid = playerid) %>%
    select(name, minor_playerid, everything())

  payload
}

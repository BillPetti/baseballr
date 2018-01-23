#' Scrape Batter Game Logs from FanGraphs
#'
#' This function allows you to scrape game logs by year for a batter from FanGraphs.com.
#' You can provide either a Fangraphs Player ID or a player name as the first argument.
#' 
#' @param playerid_or_name This is the playerid used by FanGraphs for a given player, or the player's full name
#' @param year The season for which game logs should be returned (use the YYYY format)
#' @keywords MLB, sabermetrics
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom dplyr filter rename
#' @importFrom tools toTitleCase
#' @export
#' @examples
#' \dontrun{batter_game_logs_fg(playerid_or_name = 6184, year = 2017)}
#' \dontrun{batter_game_logs_fg(playerid_or_name = "Chris Archer", year = 2016)}


batter_game_logs_fg <- function(playerid_or_name, year = 2017) {
  
  # Get player ID if not initially provided
  playerid = parse_id_or_player_name(playerid_or_name)
  
  # Fangraphs URL endpoint to look up a player's game logs
  url <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                playerid,
                "&season=",
                year,
                "&position=PB")
  
  # Process URL response and convert to a Data Frame
  payload <- xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  
  # Sort table and rename certain columns
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

parse_id_or_player_name <- function(id_or_name) {
  player_id = NA
  # Check if an ID was provided as integer or character; if so, return the already provided ID
  if (as.integer(id_or_name) == id_or_name) {
    player_id = id_or_name
  }
  # If ID not provided, try to parse a name and look up fangraphs ID
  else {
    # Split into first and last name (note: will be finicky for last names with spaces i.e. De Los Santos)
    first_name = tools::toTitleCase(tolower(strsplit(id_or_name, "\\s+")[[1]][1]))
    last_name = tools::toTitleCase(tolower(strsplit(id_or_name, "\\s+")[[1]][2]))
    player_id = playerid_lookup(last_name, first_name)[['fangraphs_id']]
  }
  player_id
}


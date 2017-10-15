#' Scrape MLB Standings on a Given Date
#'
#' This function allows you to scrape the standings from MLB for any date you choose.
#' @param date a date object
#' @param division One or more of AL East, AL Central, AL West,
#' AL Overall, NL East, NL Central, NL West, and NL Overall
#' @param from a logical indicating whether you want standings up to and
#' including the date (FALSE, default) or rather standings for games played
#' after the date
#' @keywords MLB, standings
#' @importFrom lubridate day month year
#' @importFrom rvest html_text html_nodes
#' @importFrom xml2 read_html
#' @export standings_on_date_bref
#' @examples
#' \dontrun{
#' standings_on_date_bref("2015-08-04", "AL East")
#' }

standings_on_date_bref <- function(date, division, from = FALSE) {

  stopifnot(intersect(grepl("AL|NL", division), grepl("East|Central|West|Overall",
                                                      division)))

  url <- paste0("http://www.baseball-reference.com/boxes",
                "?year=", sprintf("%04i", lubridate::year(date)), "&month=",
                sprintf("%02i", lubridate::month(date)), "&day=", sprintf("%02i",
                                                                          lubridate::day(date)))

  html_doc <- url %>% xml2::read_html()

  tables <- html_doc %>% rvest::html_nodes("table")
  min <- length(tables)
  max <- length(tables) - 15
  tables <- tables %>% .[min:max] %>% html_table
  #table_names <- html_doc %>% rvest::html_nodes(".section_heading") %>% rvest::html_text() %>% gsub(pattern = "\\s+", replacement = " ") %>% gsub(pattern = " Division", replacement = "") %>% trimws(which = c("left")) %>% trimws(which = c("right")) %>% .[1:16]

  table_names <- c("NL Overall", "AL Overall", "NL West" , "NL Central", "NL East", "AL West", "AL Central", "AL East", "NL Overall", "AL Overall", "NL West" , "NL Central", "NL East", "AL West", "AL Central", "AL East")
  table_names[1:8] <- paste0(table_names[1:8], "_after_", date)     # Customizing list names for "After this Date" case
  table_names[9:16] <- paste0(table_names[9:16], "_up to_", date)   # Customizing list names for "From this Date" case
  
  names(tables) <- table_names

  after <- tables[1:8]

  current <- tables[9:16]

  if (from == FALSE) {
    div_date <- paste0(division, "_up to_", date)
    x <- current[div_date]
  } else if (from != FALSE) {
    div_date <- paste0(division, "_after_", date)
    x <- after[div_date]
  }
  x
}

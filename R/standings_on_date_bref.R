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
#' @importFrom rvest html_text
#' @export
#' @examples
#' standings_on_date_bref("2015-08-04", "AL East")

standings_on_date_bref <- function(date, division, from = FALSE) {

  stopifnot(intersect(
    grepl("AL|NL", division), grepl("East|Central|West|Overall", division)
  ))

  url <- paste0 (
    "http://www.baseball-reference.com/games/standings.cgi",
    "?year=", sprintf("%04i", year(date)),
    "&month=", sprintf("%02i", month(date)),
    "&day=", sprintf("%02i", day(date)),
    "&submit=Submit+Date"
  )

  html_doc <- url %>% read_html

  table_names <- html_doc %>% html_nodes("h3") %>% html_text %>%
    gsub(pattern = "\\s+", replacement = " ") %>%
    gsub(pattern = " Division", replacement = "")

  tables <- html_doc %>% html_nodes(xpath = "//*[(@class = 'sortable  stats_table')]")

  ind <- match(division, table_names) + from * length(table_names) / 2

  tables %>% `[`(ind) %>% html_table %>% setNames(table_names[ind])
}

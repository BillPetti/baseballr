#' Scrape MLB Standings on a Given Date
#'
#' This function allows you to scrape the standings from MLB for any date you choose.
#' @param date a date object
#' @param division One or more of AL East, AL Central, AL West,
#' AL Overall, NL East, NL Central, NL West, and NL Overall
#' @param from a logical indicating whether you want standings up to and
#' including the date (FALSE, default) or rather standings for games played
#' after the date
#' @export standings_on_date_bref
#' @examples \donttest{
#'   standings_on_date_bref(date = "2015-08-04", division = "AL East")
#' }

standings_on_date_bref <- function(date, division, from = FALSE) {
  bref_standings_on_date(date = date, division = division, from = from)
}

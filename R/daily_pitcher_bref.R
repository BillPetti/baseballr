#' Scrape Pitcher Performance Data Over a Custom Time Frame
#'
#' This function allows you to scrape basic pitcher statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param t1 First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param t2 Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @export
#' @examples \donttest{
#'   daily_pitcher_bref("2015-05-10", "2015-06-20")
#' }

daily_pitcher_bref <- function(t1, t2) {

  bref_daily_pitcher(t1 = t1, t2 = t2)

}

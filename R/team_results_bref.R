#' Scrape Team Results
#'
#' This function allows you to scrape schedule and results for a major league team from Baseball-Reference.com
#' @param Tm The abbreviation used by Baseball-Reference.com for the team whose results you want to scrape.
#' @param year Season for which you want to scrape the park factors.
#' @export
#' @examples
#' \donttest{
#'   team_results_bref("NYM", 2015)
#'   team_results_bref(Tm="TBR", year=2008)
#' }

team_results_bref <-function(Tm, year) {

  bref_team_results(Tm = Tm, year = year)
  
}


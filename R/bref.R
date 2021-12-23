#' @name bref 
#' @title **Baseball-Reference Functions Overview** 
#' @description  
#' \describe{
#'   \item{`bref_daily_batter()`}{Scrape Batter Performance Data Over a Custom Time Frame}
#'   \item{`bref_daily_pitcher()`}{Scrape Pitcher Performance Data Over a Custom Time Frame}
#'   \item{`bref_standings_on_date()`}{Scrape MLB Standings on a Given Date}
#'   \item{`bref_team_results()`}{Scrape Team Results}
#' }
#' 
#' ### **Scrape Batter Performance Data Over a Custom Time Frame**
#' ```r
#'   bref_daily_batter("2015-05-10", "2015-06-20")
#' ```
#' ### **Scrape Pitcher Performance Data Over a Custom Time Frame**
#' ```r
#'   bref_daily_batter("2015-05-10", "2015-06-20")
#' ```
#' ### **Scrape MLB Standings on a Given Date**
#' ```r
#'   bref_standings_on_date(date = "2015-08-04", division = "AL East")
#' ```
#' ### **Scrape Team Results**
#' ```r
#'   bref_team_results("NYM", 2015)
#'   bref_team_results(Tm="TBR", year=2008)
#' ```
#' ### **Team Level Consistency** 
#' Uses `bref_team_results()` to calculate team consistency metrics
#' ```r
#'   team_consistency(year=2015)
#' ```
#' 
NULL
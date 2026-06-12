# espn_college_baseball_calendar.R
# Public NCAA college-baseball shim for the ESPN schedule-calendar endpoint.
# Thin wrapper over .espn_baseball_calendar() with league = "college-baseball";
# return shape is identical to the MLB twin, so docs are inherited via @inherit.

#' **Get ESPN College Baseball Calendar**
#' @name espn_college_baseball_calendar
NULL
#' @title
#' **Get ESPN College Baseball Calendar**
#' @rdname espn_college_baseball_calendar
#' @author Saiem Gilani
#' @inheritParams espn_mlb_calendar
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_calendar return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @importFrom janitor clean_names
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_calendar(season = 2025))
#' }
espn_college_baseball_calendar <- function(season = most_recent_college_baseball_season()) {
  .args <- mget(setdiff(names(formals()), "..."))
  .espn_baseball_calendar(
    league = "college-baseball",
    season = season
  )
}

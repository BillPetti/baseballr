#' **Get ESPN MLB Calendar**
#' @name espn_mlb_calendar
NULL
#' @title
#' **Get ESPN MLB Calendar**
#' @rdname espn_mlb_calendar
#' @author Saiem Gilani
#' @param season integer or character. Four-digit season year (e.g. `2025`).
#'   Defaults to `most_recent_mlb_season()`.
#' @return Returns a tibble of calendar entries.
#'
#'    |col_name          |types     |description                                                                                                        |
#'    |:-----------------|:---------|:------------------------------------------------------------------------------------------------------------------|
#'    |season            |character |Season identifier (4-digit year or 'YYYY-YY' string).                                                              |
#'    |season_type       |character |Season type (1=pre-season, 2=regular season, 3=postseason, 4=off-season for ESPN). |
#'    |season_type_label |character |                                                                                                                   |
#'    |season_start_date |character |Date in YYYY-MM-DD format.                                                                                         |
#'    |season_end_date   |character |Date in YYYY-MM-DD format.                                                                                         |
#'    |label             |character |                                                                                                                   |
#'    |alternate_label   |character |                                                                                                                   |
#'    |detail            |character |                                                                                                                   |
#'    |value             |character |Numeric or string value field.                                                                                     |
#'    |start_date        |character |Start date (YYYY-MM-DD).                                                                                           |
#'    |end_date          |character |End date (YYYY-MM-DD).                                                                                             |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @importFrom janitor clean_names
#' @import rvest
#' @export
#' @family ESPN MLB Functions
#' @details
#' Retrieve the ESPN MLB schedule calendar for a given season. The underlying
#' scoreboard response contains a `leagues[[1]]$calendar` block with season-type
#' entries (pre-season, regular, post). Uses `getOption("baseballr.proxy")` or
#' `http_proxy`/`https_proxy` environment variables for proxy configuration
#' (per-call proxy override is not supported for ESPN wrappers).
#' @examples
#' \donttest{
#'   espn_mlb_calendar(season = 2025)
#' }
espn_mlb_calendar <- function(season = most_recent_mlb_season()) {
  .args <- mget(setdiff(names(formals()), "..."))
  .espn_baseball_calendar(
    league = "mlb",
    season = season
  )
}

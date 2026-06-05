# espn_mlb_team_record.R

#' **Get ESPN MLB Team Record (Per Season Type)**
#' @name espn_mlb_team_record
NULL
#' @title
#' **Get ESPN MLB Team Record (Per Season Type)**
#' @rdname espn_mlb_team_record
#' @author Saiem Gilani
#' @description
#' Returns the long-format record breakdown for an MLB team in one season
#' and season-type. Each row is one record type (Overall, Home, Road,
#' vs Conference, vs Division, etc.). Backed by
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/seasons/{season}/types/{season_type}/teams/{team_id}/record`.
#'
#' @param team_id ESPN team identifier.
#' @param season Season year (numeric). Defaults to most recent MLB season.
#' @param season_type Integer (2 = regular season default; 3 = postseason).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per record category.
#'
#'    |col_name           |types     |description                                |
#'    |:------------------|:---------|:------------------------------------------|
#'    |league             |character |League slug (`"mlb"`).                     |
#'    |team_id            |character |ESPN team identifier.                      |
#'    |season             |integer   |Season year.                               |
#'    |season_type        |integer   |Season type (1/2/3).                       |
#'    |record_id          |character |Record sub-id.                             |
#'    |name               |character |Internal record name (e.g. "overall").     |
#'    |abbreviation       |character |Abbreviation (e.g. "Total").               |
#'    |display_name       |character |Display name (e.g. "Overall").             |
#'    |short_display_name |character |Short display (e.g. "OVER").               |
#'    |description        |character |Description (e.g. "Overall Record").       |
#'    |type               |character |Record type code (`total`, `home`, ...).   |
#'    |summary            |character |W-L summary (e.g. "50-32").                |
#'    |display_value      |character |Same as summary in most cases.             |
#'    |value              |numeric   |Win percentage (0-1).                      |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_team_record(team_id = 13, season = 2025)
#' }
espn_mlb_team_record <- function(team_id,
                                  season = most_recent_mlb_season(),
                                  season_type = c(2L, 3L), ...) {
  .espn_baseball_team_record(league = "mlb", team_id = team_id,
                                 season = season,
                                 season_type = season_type, ...)
}

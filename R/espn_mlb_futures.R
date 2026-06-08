# espn_mlb_futures.R
# Public MLB shim for ESPN season-futures endpoint.

# ---------------------------------------------------------------------------
# espn_mlb_futures
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Futures (Long Format)**
#' @name espn_mlb_futures
NULL
#' @title
#' **Get ESPN MLB Season Futures (Long Format)**
#' @rdname espn_mlb_futures
#' @author Saiem Gilani
#' @description
#' Returns the full futures-betting board for an MLB season, in long
#' format: one row per (market x team). Markets include championship
#' winner, conference winner, division winner, MVP odds, etc. Backed by
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/seasons/{season}/futures`.
#'
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A long-format tibble.
#'
#'    |col_name        |types     |description                                                       |
#'    |:---------------|:---------|:-----------------------------------------------------------------|
#'    |season          |integer   |Season year.                                                      |
#'    |league          |character |League slug (`"mlb"`).                                            |
#'    |market_id       |character |ESPN futures-market identifier.                                   |
#'    |market_name     |character |Internal market name (e.g. "MLB - Winner").                       |
#'    |market_type     |character |Market type code (`winLeague`, `winConference`, `winDivision`, ...).|
#'    |market_display  |character |Human-readable name (e.g. "MLB Championship Winner").             |
#'    |provider_id     |character |Sportsbook provider identifier.                                   |
#'    |provider_name   |character |Sportsbook provider name (e.g. "ESPN BET").                       |
#'    |team_id         |character |ESPN team id (parsed from `team_ref`).                            |
#'    |odds_value      |character |American odds for the team (e.g. "-250", "+800").                 |
#'    |team_ref        |character |`$ref` to the per-season team resource.                           |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_futures(season = 2025)
#' }
espn_mlb_futures <- function(season = most_recent_mlb_season(), ...) {
  .espn_baseball_futures(league = "mlb", season = season, ...)
}

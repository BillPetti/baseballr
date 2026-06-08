# espn_mlb_player_contracts.R
# Public MLB shims for ESPN athlete-contract endpoints.
# ESPN's core-v2 surface only populates contract data for MLB athletes
# (MLB athletes expose no ESPN contract index), so these wrappers are MLB-only.

# ---------------------------------------------------------------------------
# espn_mlb_player_contracts
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Contracts Index**
#' @name espn_mlb_player_contracts
NULL
#' @title
#' **Get ESPN MLB Athlete Contracts Index**
#' @rdname espn_mlb_player_contracts
#' @author Saiem Gilani
#' @description
#' Returns the index of contract seasons recorded for an MLB athlete from
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/athletes/{athlete_id}/contracts`.
#' Each row is one contract year — pass the season to
#' [espn_mlb_player_contract()] for the full contract record.
#'
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per contract year.
#'
#'    |col_name   |types     |description                                |
#'    |:----------|:---------|:------------------------------------------|
#'    |athlete_id |character |ESPN athlete identifier.                   |
#'    |season     |integer   |Contract season year.                      |
#'    |ref        |character |Full `$ref` URL for the contract detail.   |
#'    |league     |character |League slug (`"mlb"`).                     |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   # Aaron Judge — athlete id 1966
#'   espn_mlb_player_contracts(athlete_id = 33192)
#' }
espn_mlb_player_contracts <- function(athlete_id, ...) {
  .espn_baseball_athlete_contracts(
    league     = "mlb",
    athlete_id = athlete_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_mlb_player_contract
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Athlete Contract (Single Season)**
#' @name espn_mlb_player_contract
NULL
#' @title
#' **Get ESPN MLB Athlete Contract (Single Season)**
#' @rdname espn_mlb_player_contract
#' @author Saiem Gilani
#' @description
#' Returns the full contract record for one MLB athlete in one season,
#' including salary, cap-rule flags, option type, Bird status, and trade
#' protections. Backed by
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/athletes/{athlete_id}/contracts/{season}`.
#'
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name                       |types     |description                                                       |
#'    |:------------------------------|:---------|:-----------------------------------------------------------------|
#'    |athlete_id                     |character |ESPN athlete identifier.                                          |
#'    |season                         |integer   |Contract season year.                                             |
#'    |bird_status                    |integer   |Bird-rights tier (0 = Non, 1 = Early, 2 = Full).                  |
#'    |salary                         |numeric   |Total cap-counting salary for the season ($).                     |
#'    |salary_remaining               |numeric   |Remaining salary owed.                                            |
#'    |years_remaining                |integer   |Years left on the contract.                                       |
#'    |incoming_trade_value           |numeric   |Trade value if receiving this contract ($).                       |
#'    |outgoing_trade_value           |numeric   |Trade value if sending out this contract ($).                     |
#'    |option_type                    |integer   |Option type code (e.g. team / player option).                     |
#'    |minimum_salary_exception       |logical   |Signed under the minimum-salary exception.                        |
#'    |trade_restriction              |logical   |Whether a trade restriction is active.                            |
#'    |unsigned_foreign_pick          |logical   |Unsigned-foreign-pick flag.                                       |
#'    |active                         |logical   |Whether the contract is currently active.                         |
#'    |base_year_compensation_active  |logical   |Base-year-compensation rule active.                               |
#'    |poison_pill_provision_active   |logical   |Poison-pill provision active.                                     |
#'    |trade_kicker_active            |logical   |Trade kicker active.                                              |
#'    |trade_kicker_percentage        |numeric   |Trade kicker percentage of salary.                                |
#'    |trade_kicker_value             |numeric   |Trade kicker dollar value.                                        |
#'    |trade_kicker_trade_value       |numeric   |Trade kicker post-trade dollar value.                             |
#'    |season_ref                     |character |`$ref` to the season resource.                                    |
#'    |team_ref                       |character |`$ref` to the team-in-season resource.                            |
#'    |team_id                        |character |ESPN team id parsed from `team_ref`.                              |
#'    |league                         |character |League slug (`"mlb"`).                                            |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   # Aaron Judge 2025 contract
#'   espn_mlb_player_contract(athlete_id = 33192, season = 2025)
#' }
espn_mlb_player_contract <- function(athlete_id,
                                       season = most_recent_mlb_season(),
                                       ...) {
  .espn_baseball_athlete_contract(
    league     = "mlb",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

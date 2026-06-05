# espn_mlb_awards.R
# Public MLB shims for ESPN season-awards endpoints.

# ---------------------------------------------------------------------------
# espn_mlb_season_awards
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Awards Index**
#' @name espn_mlb_season_awards
NULL
#' @title
#' **Get ESPN MLB Season Awards Index**
#' @rdname espn_mlb_season_awards
#' @author Saiem Gilani
#' @description
#' Returns the list of award IDs given out in an MLB season from
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/seasons/{season}/awards`.
#' The index only contains IDs and `$ref` URLs — pass an ID to
#' [espn_mlb_award()] for the award name, description, and winners.
#'
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per award.
#'
#'    |col_name |types     |description                            |
#'    |:--------|:---------|:--------------------------------------|
#'    |season   |integer   |Season year.                           |
#'    |award_id |character |ESPN award identifier.                 |
#'    |ref      |character |Full `$ref` URL for the award detail.  |
#'    |league   |character |League slug (`"mlb"`).                 |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_season_awards(season = 2024)
#' }
espn_mlb_season_awards <- function(season = most_recent_mlb_season(), ...) {
  .espn_baseball_season_awards(league = "mlb", season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_award
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Award Detail**
#' @name espn_mlb_award
NULL
#' @title
#' **Get ESPN MLB Season Award Detail**
#' @rdname espn_mlb_award
#' @author Saiem Gilani
#' @description
#' Returns the name, description, and winners of one MLB season award.
#' Most awards have a single winner; multi-recipient awards (e.g. All-MLB
#' First Team) return one row per winner.
#'
#' @param award_id ESPN award identifier (character or numeric).
#' @param season Season year (numeric). Defaults to the most recent MLB season.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per winner.
#'
#'    |col_name    |types     |description                                              |
#'    |:-----------|:---------|:--------------------------------------------------------|
#'    |league      |character |League slug (`"mlb"`).                                   |
#'    |season      |integer   |Season year.                                             |
#'    |award_id    |character |ESPN award identifier.                                   |
#'    |name        |character |Award name (e.g. "MVP").                                 |
#'    |description |character |Award description.                                       |
#'    |athlete_id  |character |ESPN athlete id of winner (parsed from `athlete_ref`).   |
#'    |team_id     |character |ESPN team id (parsed from `team_ref`).                   |
#'    |athlete_ref |character |`$ref` to winner's per-season athlete resource.          |
#'    |team_ref    |character |`$ref` to winner's per-season team resource.             |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   # MVP award id = 33
#'   espn_mlb_award(award_id = 33, season = 2024)
#' }
espn_mlb_award <- function(award_id,
                            season = most_recent_mlb_season(),
                            ...) {
  .espn_baseball_award(
    league   = "mlb",
    season   = season,
    award_id = award_id,
    ...
  )
}

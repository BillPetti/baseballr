# espn_mlb_tournaments.R
# Public MLB shims for ESPN tournaments endpoints (e.g. the in-season
# tournament and other MLB-branded tournaments).

# ---------------------------------------------------------------------------
# espn_mlb_tournaments
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Tournaments Index**
#' @name espn_mlb_tournaments
NULL
#' @title
#' **Get ESPN MLB Tournaments Index**
#' @rdname espn_mlb_tournaments
#' @author Saiem Gilani
#' @description
#' Returns the index of MLB-branded tournaments tracked by ESPN
#' (e.g. the in-season tournament). Each row is one tournament with its
#' ID and the `$ref` URL.
#'
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per tournament.
#'
#'    |col_name      |types     |description                         |
#'    |:-------------|:---------|:-----------------------------------|
#'    |tournament_id |character |ESPN tournament identifier.         |
#'    |ref           |character |Full `$ref` URL for the detail.     |
#'    |league        |character |League slug (`"mlb"`).              |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_tournaments()
#' }
espn_mlb_tournaments <- function(...) {
  .espn_baseball_tournaments(league = "mlb", ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_tournament
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Tournament Detail**
#' @name espn_mlb_tournament
NULL
#' @title
#' **Get ESPN MLB Tournament Detail**
#' @rdname espn_mlb_tournament
#' @author Saiem Gilani
#' @description
#' Returns metadata for a single tournament plus the `$ref` URL for the
#' tournament's seasons list. Use [espn_mlb_tournament_seasons()] to
#' resolve the seasons.
#'
#' @param tournament_id ESPN tournament identifier.
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name      |types     |description                                    |
#'    |:-------------|:---------|:----------------------------------------------|
#'    |tournament_id |character |ESPN tournament identifier.                    |
#'    |display_name  |character |Human-readable tournament name.                |
#'    |seasons_ref   |character |`$ref` to the seasons-list endpoint.           |
#'    |league        |character |League slug (`"mlb"`).                         |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_tournament(tournament_id = 1)
#' }
espn_mlb_tournament <- function(tournament_id, ...) {
  .espn_baseball_tournament(league = "mlb",
                               tournament_id = tournament_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_tournament_seasons
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Tournament Seasons List**
#' @name espn_mlb_tournament_seasons
NULL
#' @title
#' **Get ESPN MLB Tournament Seasons List**
#' @rdname espn_mlb_tournament_seasons
#' @author Saiem Gilani
#' @description
#' Returns the seasons in which a given MLB tournament was held.
#'
#' @param tournament_id ESPN tournament identifier.
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per season.
#'
#'    |col_name      |types     |description                              |
#'    |:-------------|:---------|:----------------------------------------|
#'    |league        |character |League slug (`"mlb"`).                   |
#'    |tournament_id |character |ESPN tournament identifier.              |
#'    |season        |integer   |Season year.                             |
#'    |ref           |character |Full `$ref` URL for that season.         |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_tournament_seasons(tournament_id = 1)
#' }
espn_mlb_tournament_seasons <- function(tournament_id, ...) {
  .espn_baseball_tournament_seasons(league = "mlb",
                                       tournament_id = tournament_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_tournament_season
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Tournament Season Detail**
#' @name espn_mlb_tournament_season
NULL
#' @title
#' **Get ESPN MLB Tournament Season Detail**
#' @rdname espn_mlb_tournament_season
#' @author Saiem Gilani
#' @description
#' Returns single-row detail for one (tournament, season) pair: id,
#' display name, number of rounds, and `$ref`s to the season + bracketology
#' resources. Use [espn_mlb_tournament_seasons()] to enumerate valid
#' (tournament_id, season) pairs.
#'
#' @param tournament_id ESPN tournament identifier.
#' @param season Season year (numeric).
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    Note: MLB has no tournament structure at ESPN, so this returns an empty
#'    tibble (the wrapper exists for cross-sport API symmetry). When populated
#'    (other sports), the columns are:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |tournament_id |character |Unique ESPN tournament identifier. |
#'    |season |integer |Season (4-digit year). |
#'    |name |character |Tournament name. |
#'    |display_name |character |Tournament display name. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_tournament_season(tournament_id = 1, season = 2024)
#' }
espn_mlb_tournament_season <- function(tournament_id, season, ...) {
  .espn_baseball_tournament_season(league = "mlb",
                                       tournament_id = tournament_id,
                                       season = season, ...)
}

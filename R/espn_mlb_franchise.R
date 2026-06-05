# espn_mlb_franchise.R
# Public MLB shims for ESPN franchise endpoints.

# ---------------------------------------------------------------------------
# espn_mlb_franchises
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Franchises Index**
#' @name espn_mlb_franchises
NULL
#' @title
#' **Get ESPN MLB Franchises Index**
#' @rdname espn_mlb_franchises
#' @author Saiem Gilani
#' @description
#' Returns the full MLB franchises index from
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/franchises`.
#' Each row is one franchise with its ID and the canonical `$ref` URL —
#' pass an ID to [espn_mlb_franchise()] for full franchise detail.
#'
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per franchise.
#'
#'    |col_name     |types     |description                            |
#'    |:------------|:---------|:--------------------------------------|
#'    |franchise_id |character |ESPN franchise identifier.             |
#'    |ref          |character |Full `$ref` URL for franchise detail.  |
#'    |league       |character |League slug (`"mlb"`).                 |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_franchises()
#' }
espn_mlb_franchises <- function(...) {
  .espn_baseball_franchises(league = "mlb", ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_franchise
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Franchise Detail**
#' @name espn_mlb_franchise
NULL
#' @title
#' **Get ESPN MLB Franchise Detail**
#' @rdname espn_mlb_franchise
#' @author Saiem Gilani
#' @description
#' Returns franchise-level metadata for an MLB franchise. Franchise IDs are
#' stable across relocations and rebrands — useful for tracking franchise
#' history independent of current team identity.
#'
#' @param franchise_id ESPN franchise identifier (character or numeric).
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name           |types     |description                                |
#'    |:------------------|:---------|:------------------------------------------|
#'    |id                 |character |ESPN franchise identifier.                 |
#'    |uid                |character |ESPN UID string.                           |
#'    |slug               |character |URL-safe identifier.                       |
#'    |location           |character |Franchise location.                        |
#'    |name               |character |Franchise name.                            |
#'    |nickname           |character |Common nickname (often same as name).      |
#'    |abbreviation       |character |Short abbreviation.                        |
#'    |display_name       |character |Full display name.                         |
#'    |short_display_name |character |Short display name.                        |
#'    |color              |character |Primary color (hex, no leading '#').       |
#'    |is_active          |logical   |Whether franchise is currently active.     |
#'    |league             |character |League slug (`"mlb"`).                     |
#'    |logo               |character |Primary logo URL.                          |
#'    |logo_dark          |character |Dark-mode logo URL.                        |
#'    |venue_ref          |character |`$ref` to franchise's primary venue.       |
#'    |team_ref           |character |`$ref` to the current team for franchise.  |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_franchise(franchise_id = 13)
#' }
espn_mlb_franchise <- function(franchise_id, ...) {
  .espn_baseball_franchise(
    league       = "mlb",
    franchise_id = franchise_id,
    ...
  )
}

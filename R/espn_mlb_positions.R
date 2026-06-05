# espn_mlb_positions.R
# Public MLB shims for the ESPN position dictionary.

# ---------------------------------------------------------------------------
# espn_mlb_positions
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Positions Index**
#' @name espn_mlb_positions
NULL
#' @title
#' **Get ESPN MLB Positions Index**
#' @rdname espn_mlb_positions
#' @author Saiem Gilani
#' @description
#' Returns the MLB position dictionary index. One row per position with
#' its id and the canonical `$ref` URL — pass an id to
#' [espn_mlb_position()] for full details (display name, abbreviation,
#' leaf flag, parent link).
#'
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per position.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |position_id |character |Unique position identifier. |
#'    |ref |character |Ref. |
#'    |league |character |League. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_positions()
#' }
espn_mlb_positions <- function(...) {
  .espn_baseball_positions(league = "mlb", ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_position
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Position Detail**
#' @name espn_mlb_position
NULL
#' @title
#' **Get ESPN MLB Position Detail**
#' @rdname espn_mlb_position
#' @author Saiem Gilani
#' @description
#' Returns metadata for a single MLB position. Useful for dereferencing
#' position `$ref` URLs embedded in athlete records, and for navigating
#' parent/leaf relationships in the position taxonomy.
#'
#' @param position_id ESPN position identifier (character or numeric).
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |position_id |character |Unique position identifier. |
#'    |name |character |Name. |
#'    |display_name |character |Display name. |
#'    |abbreviation |character |Short abbreviation. |
#'    |leaf |logical |TRUE if a leaf position. |
#'    |parent_ref |character |Parent ref. |
#'    |league |character |League. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_position(position_id = 1)
#' }
espn_mlb_position <- function(position_id, ...) {
  .espn_baseball_position(league = "mlb",
                              position_id = position_id, ...)
}

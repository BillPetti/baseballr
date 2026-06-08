# espn_mlb_coach.R

#' **Get ESPN MLB Single-Coach Detail**
#' @name espn_mlb_coach
NULL
#' @title
#' **Get ESPN MLB Single-Coach Detail**
#' @rdname espn_mlb_coach
#' @author Saiem Gilani
#' @description
#' Returns biography, current team / college refs, and counts of career
#' record entries + per-season coaching entries for one MLB coach. Backed
#' by `sports.core.api.espn.com/v2/sports/baseball/leagues/mlb/coaches/{coach_id}`.
#'
#' @param coach_id ESPN coach identifier (character or numeric).
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name        |types     |description                                |
#'    |:---------------|:---------|:------------------------------------------|
#'    |coach_id        |character |ESPN coach identifier.                     |
#'    |uid             |character |ESPN UID string.                           |
#'    |first_name      |character |First name.                                |
#'    |last_name       |character |Last name.                                 |
#'    |date_of_birth   |character |Date of birth (ISO 8601).                  |
#'    |birth_city      |character |Birth city.                                |
#'    |birth_state     |character |Birth state / region.                      |
#'    |n_career_records|integer   |Count of career-records entries.           |
#'    |n_coach_seasons |integer   |Count of seasons coached.                  |
#'    |college_ref     |character |`$ref` to the coach's college.             |
#'    |team_ref        |character |`$ref` to the coach's current team.        |
#'    |league          |character |League slug (`"mlb"`).                     |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_coach(coach_id = 52120)
#' }
espn_mlb_coach <- function(coach_id, ...) {
  .espn_baseball_coach(league = "mlb", coach_id = coach_id, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_coach_record
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Coach Career Record (Long Format)**
#' @name espn_mlb_coach_record
NULL
#' @title
#' **Get ESPN MLB Coach Career Record (Long Format)**
#' @rdname espn_mlb_coach_record
#' @author Saiem Gilani
#' @description
#' Returns a coach's career record by type in long format (one row per
#' stat in the record's `stats[]` array). `record_type` codes commonly
#' populated: 0 = Total, 1 = Pre Season, 2 = Regular Season, 3 = Post
#' Season. Use [espn_mlb_coaches()] to discover coach_ids for a season.
#'
#' @param coach_id ESPN coach identifier (use [espn_mlb_coaches()] to find).
#' @param record_type Integer record type: 0 = Total (default), 1 = Pre Season,
#'   2 = Regular Season, 3 = Post Season.
#' @param ... Additional arguments; currently unused.
#' @return A long tibble.
#'
#'    Note: ESPN's core-v2 coach feed is sparsely populated for MLB and this
#'    frequently returns an empty tibble. When populated, the columns are:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |coach_id |character |Unique ESPN coach identifier. |
#'    |record_type |integer |Record type identifier. |
#'    |name |character |Record name (e.g. 'overall'). |
#'    |summary |character |Record summary (e.g. 'W-L'). |
#'    |stat_name |character |Statistic name. |
#'    |value |numeric |Statistic value. |
#'    |display_value |character |Human-readable statistic value. |
#'
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_coach_record(coach_id = 52120, record_type = 2)
#' }
espn_mlb_coach_record <- function(coach_id, record_type = 0L, ...) {
  .espn_baseball_coach_record(league = "mlb",
                                  coach_id = coach_id,
                                  record_type = record_type, ...)
}

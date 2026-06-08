# espn_mlb_weeks.R
# Public MLB shims for ESPN week + week-ranking endpoints.

# ---------------------------------------------------------------------------
# espn_mlb_season_weeks
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season Weeks Index**
#' @name espn_mlb_season_weeks
NULL
#' @title
#' **Get ESPN MLB Season Weeks Index**
#' @rdname espn_mlb_season_weeks
#' @author Saiem Gilani
#' @description
#' Returns the list of week IDs for one (MLB season x season-type). MLB
#' uses a week structure inherited from ESPN's schema, but week-level
#' rankings are populated only for college sports, not MLB.
#'
#' @param season Season year. Defaults to most recent MLB season.
#' @param season_type Season-type id (2 = regular (default), 3 = postseason).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per week.
#'
#'    |col_name    |types     |description                              |
#'    |:-----------|:---------|:----------------------------------------|
#'    |league      |character |League slug.                             |
#'    |season      |integer   |Season year.                             |
#'    |season_type |integer   |Season-type id.                          |
#'    |week        |integer   |Week number (1-based).                   |
#'    |ref         |character |`$ref` URL for the week detail.          |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_season_weeks(season = 2025)
#' }
espn_mlb_season_weeks <- function(season = most_recent_mlb_season(),
                                   season_type = c(2L, 3L), ...) {
  .espn_baseball_season_weeks(league = "mlb", season = season,
                                  season_type = season_type, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_season_week
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Season-Week Detail**
#' @name espn_mlb_season_week
#' @title
#' **Get ESPN MLB Season-Week Detail**
#' @rdname espn_mlb_season_week
#' @author Saiem Gilani
#' @description
#' Returns metadata for one week (number, start / end dates, text label,
#' and `$ref` to the per-week rankings endpoint).
#'
#' @param week Week number.
#' @param season Season year. Defaults to most recent MLB season.
#' @param season_type Season-type id (2 = regular (default)).
#' @param ... Additional arguments; currently unused.
#' @return A single-row tibble.
#'
#'    |col_name     |types     |description                                |
#'    |:------------|:---------|:------------------------------------------|
#'    |league       |character |League slug.                               |
#'    |season       |integer   |Season year.                               |
#'    |season_type  |integer   |Season-type id.                            |
#'    |week         |integer   |Week number.                               |
#'    |text         |character |Display label (e.g. "Week 5").             |
#'    |start_date   |character |ISO 8601 week start.                       |
#'    |end_date     |character |ISO 8601 week end.                         |
#'    |rankings_ref |character |`$ref` to the per-week rankings endpoint.  |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_season_week(week = 5, season = 2025)
#' }
espn_mlb_season_week <- function(week,
                                  season = most_recent_mlb_season(),
                                  season_type = 2L, ...) {
  .espn_baseball_season_week(league = "mlb", season = season,
                                 season_type = season_type,
                                 week = week, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_week_rankings
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Per-Week Rankings Index**
#' @name espn_mlb_week_rankings
#' @title
#' **Get ESPN MLB Per-Week Rankings Index**
#' @rdname espn_mlb_week_rankings
#' @author Saiem Gilani
#' @description
#' Returns the index of ranking sources available for one (MLB season x
#' season-type x week). MLB does not publish weekly rankings, so this
#' typically returns an empty tibble; the wrapper is provided for
#' completeness with the ESPN schema.
#'
#' @param week Week number.
#' @param season Season year. Defaults to most recent MLB season.
#' @param season_type Season-type id (2 = regular (default)).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per ranking source.
#'
#'    |col_name    |types     |description                              |
#'    |:-----------|:---------|:----------------------------------------|
#'    |league      |character |League slug.                             |
#'    |season      |integer   |Season year.                             |
#'    |season_type |integer   |Season-type id.                          |
#'    |week        |integer   |Week number.                             |
#'    |ranking_id  |character |ESPN ranking id.                         |
#'    |ref         |character |`$ref` URL for the ranked-teams detail.  |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_week_rankings(week = 5, season = 2025)
#' }
espn_mlb_week_rankings <- function(week,
                                    season = most_recent_mlb_season(),
                                    season_type = 2L, ...) {
  .espn_baseball_week_rankings(league = "mlb", season = season,
                                   season_type = season_type,
                                   week = week, ...)
}

# ---------------------------------------------------------------------------
# espn_mlb_week_ranking
# ---------------------------------------------------------------------------

#' **Get ESPN MLB Per-Week Ranking Detail**
#' @name espn_mlb_week_ranking
#' @title
#' **Get ESPN MLB Per-Week Ranking Detail**
#' @rdname espn_mlb_week_ranking
#' @author Saiem Gilani
#' @description
#' Returns the long-format ranked teams for one (season x season-type x
#' week x ranking-source). MLB typically returns an empty tibble.
#'
#' @param ranking_id Ranking source id (1 = AP, 2 = Coaches, etc.).
#' @param week Week number.
#' @param season Season year. Defaults to most recent MLB season.
#' @param season_type Season-type id (2 = regular (default)).
#' @param ... Additional arguments; currently unused.
#' @return A tibble with one row per ranked team (typically 25).
#'
#'    |col_name           |types     |description                                |
#'    |:------------------|:---------|:------------------------------------------|
#'    |league             |character |League slug.                               |
#'    |season             |integer   |Season year.                               |
#'    |season_type        |integer   |Season-type id.                            |
#'    |week               |integer   |Week number.                               |
#'    |ranking_id         |character |ESPN ranking id.                           |
#'    |name               |character |Ranking name (e.g. "AP Top 25").           |
#'    |short_name         |character |Short name.                                |
#'    |type               |character |Ranking type code.                         |
#'    |headline           |character |Full headline.                             |
#'    |date               |character |Date of the ranking.                       |
#'    |current            |integer   |Current rank.                              |
#'    |previous           |integer   |Previous-week rank.                        |
#'    |points             |numeric   |Voting points.                             |
#'    |first_place_votes  |integer   |First-place vote count.                    |
#'    |trend              |character |Trend indicator (e.g. "+3", "-2", "-").    |
#'    |record_summary     |character |Team's record at time of poll (e.g. "20-2").|
#'    |team_id            |character |ESPN team id.                              |
#'    |team_ref           |character |`$ref` to the team-in-season resource.     |
#'    |last_updated       |character |Last-updated timestamp.                    |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#'   espn_mlb_week_ranking(ranking_id = 1, week = 5, season = 2025)
#' }
espn_mlb_week_ranking <- function(ranking_id, week,
                                   season = most_recent_mlb_season(),
                                   season_type = 2L, ...) {
  .espn_baseball_week_ranking(league = "mlb", season = season,
                                  season_type = season_type,
                                  week = week,
                                  ranking_id = ranking_id, ...)
}

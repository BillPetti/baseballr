# espn_college_baseball_weeks.R
# Public NCAA college-baseball shims for ESPN week + week-ranking endpoints.
# Thin wrappers over the league-parameterized helpers in
# espn_baseball_weeks_helpers.R (the same helpers backing the espn_mlb_*week*()
# family); these fix league = "college-baseball". Return shapes are identical
# to the MLB twins, so the @return docs are inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_season_weeks
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Weeks Index**
#' @name espn_college_baseball_season_weeks
NULL
#' @title
#' **Get ESPN College Baseball Season Weeks Index**
#' @rdname espn_college_baseball_season_weeks
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_weeks
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_season_weeks return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_weeks(season = 2025))
#' }
espn_college_baseball_season_weeks <- function(season = most_recent_college_baseball_season(),
                                               season_type = c(2L, 3L), ...) {
  .espn_baseball_season_weeks(league = "college-baseball", season = season,
                                  season_type = season_type, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_week
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season-Week Detail**
#' @name espn_college_baseball_season_week
#' @title
#' **Get ESPN College Baseball Season-Week Detail**
#' @rdname espn_college_baseball_season_week
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_week
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_season_week return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_week(week = 5, season = 2025))
#' }
espn_college_baseball_season_week <- function(week,
                                              season = most_recent_college_baseball_season(),
                                              season_type = 2L, ...) {
  .espn_baseball_season_week(league = "college-baseball", season = season,
                                 season_type = season_type,
                                 week = week, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_week_rankings
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Per-Week Rankings Index**
#' @name espn_college_baseball_week_rankings
#' @title
#' **Get ESPN College Baseball Per-Week Rankings Index**
#' @rdname espn_college_baseball_week_rankings
#' @author Saiem Gilani
#' @inheritParams espn_mlb_week_rankings
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_week_rankings return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_week_rankings(week = 5, season = 2025))
#' }
espn_college_baseball_week_rankings <- function(week,
                                                season = most_recent_college_baseball_season(),
                                                season_type = 2L, ...) {
  .espn_baseball_week_rankings(league = "college-baseball", season = season,
                                   season_type = season_type,
                                   week = week, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_week_ranking
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Per-Week Ranking Detail**
#' @name espn_college_baseball_week_ranking
#' @title
#' **Get ESPN College Baseball Per-Week Ranking Detail**
#' @rdname espn_college_baseball_week_ranking
#' @author Saiem Gilani
#' @inheritParams espn_mlb_week_ranking
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_week_ranking return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_week_ranking(ranking_id = 1, week = 5, season = 2025))
#' }
espn_college_baseball_week_ranking <- function(ranking_id, week,
                                               season = most_recent_college_baseball_season(),
                                               season_type = 2L, ...) {
  .espn_baseball_week_ranking(league = "college-baseball", season = season,
                                  season_type = season_type,
                                  week = week,
                                  ranking_id = ranking_id, ...)
}

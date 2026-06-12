# espn_college_baseball_season_meta.R
# Public NCAA college-baseball shims for ESPN season-metadata endpoints:
# types, leaders, rankings. Thin wrappers over the league-parameterized
# helpers backing the espn_mlb_season_*() family; these fix
# league = "college-baseball". Return shapes match the MLB twins, so the
# @return docs are inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_season_types
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Types Index**
#' @name espn_college_baseball_season_types
NULL
#' @title
#' **Get ESPN College Baseball Season Types Index**
#' @rdname espn_college_baseball_season_types
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_types
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_season_types return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_types(season = 2025))
#' }
espn_college_baseball_season_types <- function(season = most_recent_college_baseball_season(), ...) {
  .espn_baseball_season_types(league = "college-baseball", season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_type
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season-Type Detail**
#' @name espn_college_baseball_season_type
NULL
#' @title
#' **Get ESPN College Baseball Season-Type Detail**
#' @rdname espn_college_baseball_season_type
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_type
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_season_type return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_type(season_type = 2, season = 2025))
#' }
espn_college_baseball_season_type <- function(season_type = 2L,
                                              season = most_recent_college_baseball_season(),
                                              ...) {
  .espn_baseball_season_type(league = "college-baseball", season = season,
                             season_type = season_type, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_leaders
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Leaders (Long Format)**
#' @name espn_college_baseball_season_leaders
NULL
#' @title
#' **Get ESPN College Baseball Season Leaders (Long Format)**
#' @rdname espn_college_baseball_season_leaders
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_leaders
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_season_leaders return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_leaders(season = 2025))
#' }
espn_college_baseball_season_leaders <- function(season = most_recent_college_baseball_season(),
                                                 season_type = c(2L, 3L), ...) {
  .espn_baseball_season_leaders(league = "college-baseball", season = season,
                                season_type = season_type, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_rankings
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Rankings Index**
#' @name espn_college_baseball_season_rankings
NULL
#' @title
#' **Get ESPN College Baseball Season Rankings Index**
#' @rdname espn_college_baseball_season_rankings
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_rankings
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_season_rankings return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_rankings(season = 2025))
#' }
espn_college_baseball_season_rankings <- function(season = most_recent_college_baseball_season(), ...) {
  .espn_baseball_season_rankings(league = "college-baseball", season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_ranking
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Ranking Detail**
#' @name espn_college_baseball_season_ranking
NULL
#' @title
#' **Get ESPN College Baseball Season Ranking Detail**
#' @rdname espn_college_baseball_season_ranking
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_ranking
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_season_ranking return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_ranking(ranking_id = 1, season = 2025))
#' }
espn_college_baseball_season_ranking <- function(ranking_id,
                                                 season = most_recent_college_baseball_season(),
                                                 ...) {
  .espn_baseball_season_ranking(league = "college-baseball", season = season,
                                ranking_id = ranking_id, ...)
}

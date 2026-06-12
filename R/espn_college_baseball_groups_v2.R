# espn_college_baseball_groups_v2.R
# Public NCAA college-baseball shims for ESPN per-season group / conference
# endpoints. Thin wrappers over the league-parameterized helpers backing the
# espn_mlb_season_group*() family; these fix league = "college-baseball".
# Return shapes are identical to the MLB twins, so the @return docs are
# inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_season_groups
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Groups Index**
#' @name espn_college_baseball_season_groups
NULL
#' @title
#' **Get ESPN College Baseball Season Groups Index**
#' @rdname espn_college_baseball_season_groups
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_groups
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_season_groups return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_groups(season = 2025))
#' }
espn_college_baseball_season_groups <- function(season = most_recent_college_baseball_season(),
                                                season_type = c(2L, 3L), ...) {
  .espn_baseball_season_groups(league = "college-baseball", season = season,
                                   season_type = season_type, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_group
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Group Detail**
#' @name espn_college_baseball_season_group
NULL
#' @title
#' **Get ESPN College Baseball Season Group Detail**
#' @rdname espn_college_baseball_season_group
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_group
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_season_group return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_group(group_id = 5, season = 2025))
#' }
espn_college_baseball_season_group <- function(group_id,
                                               season = most_recent_college_baseball_season(),
                                               season_type = 2L, ...) {
  .espn_baseball_season_group(league = "college-baseball", season = season,
                                  season_type = season_type,
                                  group_id = group_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_group_children
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Group Children Index**
#' @name espn_college_baseball_season_group_children
NULL
#' @title
#' **Get ESPN College Baseball Season Group Children Index**
#' @rdname espn_college_baseball_season_group_children
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_group_children
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_season_group_children return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_group_children(group_id = 5, season = 2025))
#' }
espn_college_baseball_season_group_children <- function(group_id,
                                                        season = most_recent_college_baseball_season(),
                                                        season_type = 2L, ...) {
  .espn_baseball_season_group_children(league = "college-baseball", season = season,
                                           season_type = season_type,
                                           group_id = group_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_group_teams
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Group Teams Index**
#' @name espn_college_baseball_season_group_teams
NULL
#' @title
#' **Get ESPN College Baseball Season Group Teams Index**
#' @rdname espn_college_baseball_season_group_teams
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_group_teams
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_season_group_teams return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_group_teams(group_id = 5, season = 2025))
#' }
espn_college_baseball_season_group_teams <- function(group_id,
                                                     season = most_recent_college_baseball_season(),
                                                     season_type = 2L, ...) {
  .espn_baseball_season_group_teams(league = "college-baseball", season = season,
                                        season_type = season_type,
                                        group_id = group_id, ...)
}

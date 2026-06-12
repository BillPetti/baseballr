# espn_college_baseball_team_detail.R
# Public NCAA college-baseball shims for ESPN team-detail endpoints.
# Thin wrappers over the league-parameterized helpers in
# espn_baseball_team_helpers.R (the same helpers backing the espn_mlb_team_*()
# family); these fix league = "college-baseball". Return shapes are identical
# to the MLB twins, so the @return docs are inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_team
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Team Detail**
#' @name espn_college_baseball_team
NULL
#' @title
#' **Get ESPN College Baseball Team Detail**
#' @rdname espn_college_baseball_team
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team
#' @param season Season year (numeric, e.g. 2025). Defaults to the most recent
#'   college baseball season.
#' @inherit espn_mlb_team return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble select any_of bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   espn_college_baseball_team(team_id = "59", season = 2025)
#' }
espn_college_baseball_team <- function(team_id,
                                       season = most_recent_college_baseball_season(),
                                       ...) {
  .espn_baseball_team(
    league  = "college-baseball",
    team_id = team_id,
    season  = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_team_roster
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Team Roster**
#' @name espn_college_baseball_team_roster
NULL
#' @title
#' **Get ESPN College Baseball Team Roster**
#' @rdname espn_college_baseball_team_roster
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team_roster
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_team_roster return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   espn_college_baseball_team_roster(team_id = "59", season = 2025)
#' }
espn_college_baseball_team_roster <- function(team_id,
                                              season = most_recent_college_baseball_season(),
                                              ...) {
  .espn_baseball_team_roster(
    league  = "college-baseball",
    team_id = team_id,
    season  = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_team_schedule
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Team Schedule**
#' @name espn_college_baseball_team_schedule
NULL
#' @title
#' **Get ESPN College Baseball Team Schedule**
#' @rdname espn_college_baseball_team_schedule
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team_schedule
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_team_schedule return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   espn_college_baseball_team_schedule(team_id = "59", season = 2025)
#' }
espn_college_baseball_team_schedule <- function(team_id,
                                                season      = most_recent_college_baseball_season(),
                                                season_type = 2,
                                                ...) {
  .espn_baseball_team_schedule(
    league      = "college-baseball",
    team_id     = team_id,
    season      = season,
    season_type = season_type,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_team_leaders
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Team Leaders**
#' @name espn_college_baseball_team_leaders
NULL
#' @title
#' **Get ESPN College Baseball Team Leaders**
#' @rdname espn_college_baseball_team_leaders
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team_leaders
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_team_leaders return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   espn_college_baseball_team_leaders(team_id = "59", season = 2025)
#' }
espn_college_baseball_team_leaders <- function(team_id,
                                               season = most_recent_college_baseball_season(),
                                               ...) {
  .espn_baseball_team_leaders(
    league  = "college-baseball",
    team_id = team_id,
    season  = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_team_season_profile
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Team-in-Season Profile**
#' @name espn_college_baseball_team_season_profile
NULL
#' @title
#' **Get ESPN College Baseball Team-in-Season Profile**
#' @rdname espn_college_baseball_team_season_profile
#' @author Saiem Gilani
#' @description
#' Era-correct team identity for an NCAA college baseball program in a specific
#' season, plus the available `$ref` URLs for deeper resources (record,
#' statistics, leaders, coaches, etc.). Backed by the core-v2 endpoint
#' `sports.core.api.espn.com/v2/sports/baseball/leagues/college-baseball/seasons/{season}/teams/{team_id}`.
#' @inheritParams espn_mlb_team_season_profile
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_team_season_profile return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   espn_college_baseball_team_season_profile(team_id = "59", season = 2025)
#' }
espn_college_baseball_team_season_profile <- function(team_id,
                                                      season = most_recent_college_baseball_season(),
                                                      ...) {
  .espn_baseball_team_season_profile(
    league  = "college-baseball",
    team_id = team_id,
    season  = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_team_season_statistics
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Team Season Statistics (Long Format with Rank)**
#' @name espn_college_baseball_team_season_statistics
NULL
#' @title
#' **Get ESPN College Baseball Team Season Statistics (Long Format with Rank)**
#' @rdname espn_college_baseball_team_season_statistics
#' @author Saiem Gilani
#' @description
#' Returns the full team-season-type statistics sheet for one college baseball
#' team in long format: one row per (category x stat), each carrying the team's
#' league rank for that stat where ESPN provides it.
#' @inheritParams espn_mlb_team_season_statistics
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_team_season_statistics return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   espn_college_baseball_team_season_statistics(team_id = 59, season = 2025)
#' }
espn_college_baseball_team_season_statistics <- function(team_id,
                                                         season      = most_recent_college_baseball_season(),
                                                         season_type = 2L,
                                                         ...) {
  .espn_baseball_team_season_statistics(
    league      = "college-baseball",
    team_id     = team_id,
    season      = season,
    season_type = season_type,
    ...
  )
}

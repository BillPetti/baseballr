# espn_college_baseball_team_deep.R
# Public NCAA college-baseball shims for deeper per-team / per-coach core-v2
# endpoints. Thin wrappers over the league-parameterized helpers in
# espn_baseball_team_helpers.R (the same helpers backing the espn_mlb_team_*()
# family); these fix league = "college-baseball". Return shapes are identical
# to the MLB twins, so the @return docs are inherited via @inherit. The
# pro/betting-only twins (team_odds_records, team_depthchart) are intentionally
# omitted -- ESPN does not publish that data for college baseball.

# ---------------------------------------------------------------------------
# espn_college_baseball_team_season_roster
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Team Roster (Per-Season, core-v2)**
#' @name espn_college_baseball_team_season_roster
#' @title
#' **Get ESPN College Baseball Team Roster (Per-Season, core-v2)**
#' @rdname espn_college_baseball_team_season_roster
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team_season_roster
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_team_season_roster return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_team_season_roster(team_id = "59", season = 2025))
#' }
espn_college_baseball_team_season_roster <- function(team_id,
                                                     season = most_recent_college_baseball_season(),
                                                     ...) {
  .espn_baseball_team_season_roster(league = "college-baseball", team_id = team_id,
                                        season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_coach_season
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Coach-in-Season Detail**
#' @name espn_college_baseball_coach_season
#' @title
#' **Get ESPN College Baseball Coach-in-Season Detail**
#' @rdname espn_college_baseball_coach_season
#' @author Saiem Gilani
#' @inheritParams espn_mlb_coach_season
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_coach_season return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_coach_season(coach_id = 52120, season = 2025))
#' }
espn_college_baseball_coach_season <- function(coach_id,
                                               season = most_recent_college_baseball_season(),
                                               ...) {
  .espn_baseball_coach_season(league = "college-baseball", coach_id = coach_id,
                                  season = season, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_team_record_detail
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Team Record Detail (Long Format)**
#' @name espn_college_baseball_team_record_detail
NULL
#' @title
#' **Get ESPN College Baseball Team Record Detail (Long Format)**
#' @rdname espn_college_baseball_team_record_detail
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team_record_detail
#' @inherit espn_mlb_team_record_detail return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_team_record_detail(team_id = "59", season = 2025, record_id = 0))
#' }
espn_college_baseball_team_record_detail <- function(team_id, season, record_id,
                                                     season_type = 2L, ...) {
  .espn_baseball_team_record_detail(league = "college-baseball",
                                        team_id = team_id,
                                        season = season,
                                        record_id = record_id,
                                        season_type = season_type, ...)
}

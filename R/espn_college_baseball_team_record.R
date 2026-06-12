# espn_college_baseball_team_record.R
# Public NCAA college-baseball shim for the ESPN team-record endpoint.
# Thin wrapper over the league-parameterized helper in
# espn_baseball_team_helpers.R (the same helper backing espn_mlb_team_record());
# this fixes league = "college-baseball". Return shape is identical to the MLB
# twin, so the @return docs are inherited via @inherit.

#' **Get ESPN College Baseball Team Record (Per Season Type)**
#' @name espn_college_baseball_team_record
NULL
#' @title
#' **Get ESPN College Baseball Team Record (Per Season Type)**
#' @rdname espn_college_baseball_team_record
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team_record
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_team_record return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_team_record(team_id = "59", season = 2025))
#' }
espn_college_baseball_team_record <- function(team_id,
                                              season = most_recent_college_baseball_season(),
                                              season_type = c(2L, 3L), ...) {
  .espn_baseball_team_record(league = "college-baseball", team_id = team_id,
                                 season = season,
                                 season_type = season_type, ...)
}

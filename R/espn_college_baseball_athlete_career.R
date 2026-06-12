# espn_college_baseball_athlete_career.R
# Public NCAA college-baseball shims for athlete career-level core-v2
# endpoints. Thin wrappers over the league-parameterized helpers in
# espn_baseball_athlete_helpers.R (the same helpers backing the
# espn_mlb_player_seasons() / espn_mlb_player_career_stats() functions);
# these fix league = "college-baseball". Return shapes are identical to the
# MLB twins, so the @return docs are inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_player_seasons
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Career Seasons**
#' @name espn_college_baseball_player_seasons
NULL
#' @title
#' **Get ESPN College Baseball Athlete Career Seasons**
#' @rdname espn_college_baseball_player_seasons
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_seasons
#' @inherit espn_mlb_player_seasons return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_seasons(athlete_id = "3730"))
#' }
espn_college_baseball_player_seasons <- function(athlete_id, ...) {
  .espn_baseball_athlete_seasons(league = "college-baseball",
                                     athlete_id = athlete_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_player_career_stats
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Career Stats (Long Format)**
#' @name espn_college_baseball_player_career_stats
NULL
#' @title
#' **Get ESPN College Baseball Athlete Career Stats (Long Format)**
#' @rdname espn_college_baseball_player_career_stats
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_career_stats
#' @inherit espn_mlb_player_career_stats return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_career_stats(athlete_id = "3730"))
#'   try(espn_college_baseball_player_career_stats(athlete_id = "3730", stat_type = 2L))
#' }
espn_college_baseball_player_career_stats <- function(athlete_id,
                                                      stat_type = 0L,
                                                      ...) {
  .espn_baseball_athlete_career_stats(league = "college-baseball",
                                          athlete_id = athlete_id,
                                          stat_type = stat_type, ...)
}

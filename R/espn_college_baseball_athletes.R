# espn_college_baseball_athletes.R
# Public NCAA college-baseball shims for ESPN athlete endpoints.
# Thin wrappers over the league-parameterized helpers in
# espn_baseball_athlete_helpers.R (the same helpers backing the
# espn_mlb_player_*() family); these fix league = "college-baseball".
# Return shapes are identical to the MLB twins, so the @return docs are
# inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_player_info
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Info**
#' @name espn_college_baseball_player_info
NULL
#' @title
#' **Get ESPN College Baseball Athlete Info**
#' @rdname espn_college_baseball_player_info
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_info
#' @inherit espn_mlb_player_info return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble select any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_info(athlete_id = "3730"))
#' }
espn_college_baseball_player_info <- function(athlete_id, ...) {
  .espn_baseball_athlete_info(
    league     = "college-baseball",
    athlete_id = athlete_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_player_overview
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Overview**
#' @name espn_college_baseball_player_overview
NULL
#' @title
#' **Get ESPN College Baseball Athlete Overview**
#' @rdname espn_college_baseball_player_overview
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_overview
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_player_overview return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble select any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_overview(athlete_id = "3730", season = 2025))
#' }
espn_college_baseball_player_overview <- function(athlete_id,
                                                  season = most_recent_college_baseball_season(),
                                                  ...) {
  .espn_baseball_athlete_overview(
    league     = "college-baseball",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_player_gamelog
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Gamelog**
#' @name espn_college_baseball_player_gamelog
NULL
#' @title
#' **Get ESPN College Baseball Athlete Gamelog**
#' @rdname espn_college_baseball_player_gamelog
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_gamelog
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_player_gamelog return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_gamelog(athlete_id = "3730", season = 2025))
#' }
espn_college_baseball_player_gamelog <- function(athlete_id,
                                                 season = most_recent_college_baseball_season(),
                                                 ...) {
  .espn_baseball_athlete_gamelog(
    league     = "college-baseball",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_player_splits
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Splits**
#' @name espn_college_baseball_player_splits
NULL
#' @title
#' **Get ESPN College Baseball Athlete Splits**
#' @rdname espn_college_baseball_player_splits
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_splits
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_player_splits return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows select any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_splits(athlete_id = "3730", season = 2025))
#' }
espn_college_baseball_player_splits <- function(athlete_id,
                                                season = most_recent_college_baseball_season(),
                                                ...) {
  .espn_baseball_athlete_splits(
    league     = "college-baseball",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_player_eventlog
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Eventlog**
#' @name espn_college_baseball_player_eventlog
NULL
#' @title
#' **Get ESPN College Baseball Athlete Eventlog**
#' @rdname espn_college_baseball_player_eventlog
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_eventlog
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_player_eventlog return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_eventlog(athlete_id = "3730", season = 2025))
#' }
espn_college_baseball_player_eventlog <- function(athlete_id,
                                                  season = most_recent_college_baseball_season(),
                                                  ...) {
  .espn_baseball_athlete_eventlog(
    league     = "college-baseball",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_player_awards
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Awards**
#' @name espn_college_baseball_player_awards
NULL
#' @title
#' **Get ESPN College Baseball Athlete Awards**
#' @rdname espn_college_baseball_player_awards
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_awards
#' @inherit espn_mlb_player_awards return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble select any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_awards(athlete_id = "3730"))
#' }
espn_college_baseball_player_awards <- function(athlete_id, ...) {
  .espn_baseball_athlete_awards(
    league     = "college-baseball",
    athlete_id = athlete_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_player_statisticslog
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athlete Statisticslog**
#' @name espn_college_baseball_player_statisticslog
NULL
#' @title
#' **Get ESPN College Baseball Athlete Statisticslog**
#' @rdname espn_college_baseball_player_statisticslog
#' @author Saiem Gilani
#' @inheritParams espn_mlb_player_statisticslog
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_player_statisticslog return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_player_statisticslog(athlete_id = "3730", season = 2025))
#' }
espn_college_baseball_player_statisticslog <- function(athlete_id,
                                                       season = most_recent_college_baseball_season(),
                                                       ...) {
  .espn_baseball_athlete_statisticslog(
    league     = "college-baseball",
    athlete_id = athlete_id,
    season     = season,
    ...
  )
}

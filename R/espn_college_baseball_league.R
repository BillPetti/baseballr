# espn_college_baseball_league.R
# Public NCAA college-baseball shims for ESPN league-wide catalog endpoints.
# Thin wrappers over the league-parameterized helpers in
# espn_baseball_league_helpers.R (the same helpers backing the espn_mlb_*()
# family); these fix league = "college-baseball". Return shapes are identical
# to the MLB twins, so the @return docs are inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_leaders
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball League Leaders**
#' @name espn_college_baseball_leaders
NULL
#' @title
#' **Get ESPN College Baseball League Leaders**
#' @rdname espn_college_baseball_leaders
#' @author Saiem Gilani
#' @inheritParams espn_mlb_leaders
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_leaders return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_leaders(season = 2025, season_type = 2))
#' }
espn_college_baseball_leaders <- function(season      = most_recent_college_baseball_season(),
                                          season_type = 2,
                                          ...) {
  .espn_baseball_leaders(
    league      = "college-baseball",
    season      = season,
    season_type = season_type,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_venues
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Venues**
#' @name espn_college_baseball_venues
NULL
#' @title
#' **Get ESPN College Baseball Venues**
#' @rdname espn_college_baseball_venues
#' @author Saiem Gilani
#' @inheritParams espn_mlb_venues
#' @inherit espn_mlb_venues return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_venues())
#' }
espn_college_baseball_venues <- function(...) {
  .espn_baseball_venues(
    league = "college-baseball",
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_coaches
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Coaches**
#' @name espn_college_baseball_coaches
NULL
#' @title
#' **Get ESPN College Baseball Coaches**
#' @rdname espn_college_baseball_coaches
#' @author Saiem Gilani
#' @inheritParams espn_mlb_coaches
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_coaches return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_coaches(season = 2025))
#' }
espn_college_baseball_coaches <- function(season = most_recent_college_baseball_season(),
                                          ...) {
  .espn_baseball_coaches(
    league = "college-baseball",
    season = season,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_athletes_index
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Athletes Index**
#' @name espn_college_baseball_athletes_index
NULL
#' @title
#' **Get ESPN College Baseball Athletes Index**
#' @rdname espn_college_baseball_athletes_index
#' @author Saiem Gilani
#' @inheritParams espn_mlb_athletes_index
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_athletes_index return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble bind_rows any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_athletes_index(season = 2025, limit = 50))
#' }
espn_college_baseball_athletes_index <- function(season = most_recent_college_baseball_season(),
                                                 active = TRUE,
                                                 limit  = 5000L,
                                                 ...) {
  .espn_baseball_athletes_index(
    league = "college-baseball",
    season = season,
    active = active,
    limit  = limit,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_seasons
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Seasons**
#' @name espn_college_baseball_seasons
NULL
#' @title
#' **Get ESPN College Baseball Seasons**
#' @rdname espn_college_baseball_seasons
#' @author Saiem Gilani
#' @inheritParams espn_mlb_seasons
#' @inherit espn_mlb_seasons return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_seasons())
#' }
espn_college_baseball_seasons <- function(...) {
  .espn_baseball_seasons(
    league = "college-baseball",
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_season_info
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Season Info**
#' @name espn_college_baseball_season_info
NULL
#' @title
#' **Get ESPN College Baseball Season Info**
#' @rdname espn_college_baseball_season_info
#' @author Saiem Gilani
#' @inheritParams espn_mlb_season_info
#' @param season Season year (numeric). Defaults to the most recent college baseball season.
#' @inherit espn_mlb_season_info return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble any_of
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_season_info(season = 2025))
#' }
espn_college_baseball_season_info <- function(season = most_recent_college_baseball_season(),
                                              ...) {
  .espn_baseball_season_info(
    league = "college-baseball",
    season = season,
    ...
  )
}

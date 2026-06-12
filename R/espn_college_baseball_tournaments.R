# espn_college_baseball_tournaments.R
# Public NCAA college-baseball shims for ESPN tournaments endpoints (e.g. the
# College World Series and other college-baseball postseason tournaments).
# Thin wrappers over the league-parameterized helpers in
# espn_baseball_tournaments_helpers.R (the same helpers backing the
# espn_mlb_tournament_*() family); these fix league = "college-baseball".
# Return shapes are identical to the MLB twins, so the @return docs are
# inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_tournaments
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Tournaments Index**
#' @name espn_college_baseball_tournaments
NULL
#' @title
#' **Get ESPN College Baseball Tournaments Index**
#' @rdname espn_college_baseball_tournaments
#' @author Saiem Gilani
#' @inheritParams espn_mlb_tournaments
#' @inherit espn_mlb_tournaments return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_tournaments())
#' }
espn_college_baseball_tournaments <- function(...) {
  .espn_baseball_tournaments(league = "college-baseball", ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_tournament
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Tournament Detail**
#' @name espn_college_baseball_tournament
NULL
#' @title
#' **Get ESPN College Baseball Tournament Detail**
#' @rdname espn_college_baseball_tournament
#' @author Saiem Gilani
#' @inheritParams espn_mlb_tournament
#' @inherit espn_mlb_tournament return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_tournament(tournament_id = 1))
#' }
espn_college_baseball_tournament <- function(tournament_id, ...) {
  .espn_baseball_tournament(league = "college-baseball",
                               tournament_id = tournament_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_tournament_seasons
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Tournament Seasons List**
#' @name espn_college_baseball_tournament_seasons
NULL
#' @title
#' **Get ESPN College Baseball Tournament Seasons List**
#' @rdname espn_college_baseball_tournament_seasons
#' @author Saiem Gilani
#' @inheritParams espn_mlb_tournament_seasons
#' @inherit espn_mlb_tournament_seasons return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_tournament_seasons(tournament_id = 1))
#' }
espn_college_baseball_tournament_seasons <- function(tournament_id, ...) {
  .espn_baseball_tournament_seasons(league = "college-baseball",
                                       tournament_id = tournament_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_tournament_season
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Tournament Season Detail**
#' @name espn_college_baseball_tournament_season
NULL
#' @title
#' **Get ESPN College Baseball Tournament Season Detail**
#' @rdname espn_college_baseball_tournament_season
#' @author Saiem Gilani
#' @inheritParams espn_mlb_tournament_season
#' @param season Season year (numeric). Defaults to the most recent college
#'   baseball season.
#' @inherit espn_mlb_tournament_season return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_tournament_season(tournament_id = 1, season = 2025))
#' }
espn_college_baseball_tournament_season <- function(tournament_id,
                                                    season = most_recent_college_baseball_season(),
                                                    ...) {
  .espn_baseball_tournament_season(league = "college-baseball",
                                       tournament_id = tournament_id,
                                       season = season, ...)
}

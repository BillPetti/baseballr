# espn_college_baseball_coach.R
# Public NCAA college-baseball shims for ESPN coach endpoints.
# Thin wrappers over the league-parameterized helpers in
# espn_baseball_coach_helpers.R (the same helpers backing the espn_mlb_coach*()
# family); these fix league = "college-baseball". Return shapes are identical
# to the MLB twins, so the @return docs are inherited via @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_coach
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Single-Coach Detail**
#' @name espn_college_baseball_coach
NULL
#' @title
#' **Get ESPN College Baseball Single-Coach Detail**
#' @rdname espn_college_baseball_coach
#' @author Saiem Gilani
#' @inheritParams espn_mlb_coach
#' @inherit espn_mlb_coach return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_coach(coach_id = 52120))
#' }
espn_college_baseball_coach <- function(coach_id, ...) {
  .espn_baseball_coach(league = "college-baseball", coach_id = coach_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_coach_record
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Coach Career Record (Long Format)**
#' @name espn_college_baseball_coach_record
NULL
#' @title
#' **Get ESPN College Baseball Coach Career Record (Long Format)**
#' @rdname espn_college_baseball_coach_record
#' @author Saiem Gilani
#' @inheritParams espn_mlb_coach_record
#' @inherit espn_mlb_coach_record return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_coach_record(coach_id = 52120, record_type = 2))
#' }
espn_college_baseball_coach_record <- function(coach_id, record_type = 0L, ...) {
  .espn_baseball_coach_record(league = "college-baseball",
                                  coach_id = coach_id,
                                  record_type = record_type, ...)
}

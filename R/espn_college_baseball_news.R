# espn_college_baseball_news.R
# Public NCAA college-baseball shims for the ESPN news endpoints. Thin wrappers
# over the league-parameterized helpers backing espn_mlb_news() /
# espn_mlb_team_news() with league = "college-baseball"; return shapes are
# identical to the MLB twins, so docs are inherited via @inherit.

#' **Get ESPN College Baseball News**
#' @name espn_college_baseball_news
NULL
#' @title
#' **Get ESPN College Baseball News**
#' @rdname espn_college_baseball_news
#' @author Saiem Gilani
#' @inheritParams espn_mlb_news
#' @inherit espn_mlb_news return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select as_tibble
#' @importFrom janitor clean_names
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_news(limit = 5))
#' }
espn_college_baseball_news <- function(limit = 50) {
  .args <- mget(setdiff(names(formals()), "..."))
  .espn_baseball_news(league = "college-baseball", limit = limit)
}


#' **Get ESPN College Baseball Team News**
#' @name espn_college_baseball_team_news
NULL
#' @title
#' **Get ESPN College Baseball Team News**
#' @rdname espn_college_baseball_team_news
#' @author Saiem Gilani
#' @inheritParams espn_mlb_team_news
#' @inherit espn_mlb_team_news return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select as_tibble
#' @importFrom janitor clean_names
#' @import rvest
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_team_news(team_id = "59", limit = 5))
#' }
espn_college_baseball_team_news <- function(team_id, limit = 25) {
  .args <- mget(setdiff(names(formals()), "..."))
  .espn_baseball_team_news(
    league  = "college-baseball",
    team_id = team_id,
    limit   = limit
  )
}

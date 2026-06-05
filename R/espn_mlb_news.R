#' **Get ESPN MLB News**
#' @name espn_mlb_news
NULL
#' @title
#' **Get ESPN MLB News**
#' @rdname espn_mlb_news
#' @author Saiem Gilani
#' @param limit integer. Maximum number of articles to return. Default `50`.
#' @return Returns a tibble of news articles.
#'
#'    |col_name    |types     |description                       |
#'    |:-----------|:---------|:---------------------------------|
#'    |id          |character |Id.                               |
#'    |type        |character |Record type / category.           |
#'    |headline    |character |News headline.                    |
#'    |description |character |Long-form description text.       |
#'    |published   |character |Publication timestamp (ISO 8601). |
#'    |premium     |logical   |                                  |
#'    |byline      |character |News article byline / author.     |
#'    |link_web    |character |Web link / URL.                   |
#'    |league_id   |character |ESPN league identifier.  |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select as_tibble
#' @importFrom janitor clean_names
#' @import rvest
#' @export
#' @family ESPN MLB Functions
#' @details
#' Retrieve ESPN MLB news. Uses `getOption("baseballr.proxy")` or
#' `http_proxy`/`https_proxy` environment variables for proxy configuration
#' (per-call proxy override is not supported for ESPN wrappers).
#' @examples
#' \donttest{
#'   espn_mlb_news(limit = 5)
#' }
espn_mlb_news <- function(limit = 50) {
  .args <- mget(setdiff(names(formals()), "..."))
  .espn_baseball_news(league = "mlb", limit = limit)
}


#' **Get ESPN MLB Team News**
#' @name espn_mlb_team_news
NULL
#' @title
#' **Get ESPN MLB Team News**
#' @rdname espn_mlb_team_news
#' @author Saiem Gilani
#' @param team_id character or integer. ESPN team ID (e.g. `17` for
#'   New York Yankees).
#' @param limit integer. Maximum number of articles to return. Default `25`.
#' @return Returns a tibble of team news articles.
#'
#'    |col_name    |types     |description                       |
#'    |:-----------|:---------|:---------------------------------|
#'    |id          |character |Id.                               |
#'    |type        |character |Record type / category.           |
#'    |headline    |character |News headline.                    |
#'    |description |character |Long-form description text.       |
#'    |published   |character |Publication timestamp (ISO 8601). |
#'    |premium     |logical   |                                  |
#'    |byline      |character |News article byline / author.     |
#'    |link_web    |character |Web link / URL.                   |
#'    |league_id   |character |ESPN league identifier.  |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select as_tibble
#' @importFrom janitor clean_names
#' @import rvest
#' @export
#' @family ESPN MLB Functions
#' @details
#' Retrieve ESPN MLB news for a specific team. Uses
#' `getOption("baseballr.proxy")` or `http_proxy`/`https_proxy` environment
#' variables for proxy configuration (per-call proxy override is not
#' supported for ESPN wrappers).
#' @examples
#' \donttest{
#'   espn_mlb_team_news(team_id = "13", limit = 5)
#' }
espn_mlb_team_news <- function(team_id, limit = 25) {
  .args <- mget(setdiff(names(formals()), "..."))
  .espn_baseball_team_news(
    league  = "mlb",
    team_id = team_id,
    limit   = limit
  )
}

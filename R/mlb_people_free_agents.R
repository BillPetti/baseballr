#' @title **Find Information About MLB Free Agents**
#' @param season Season preceding free agency
#' @return Returns a tibble with the following columns:
#'
#'  |col_name               |types      |
#'  |:----------------------|:----------|
#'  | date_declared         | character |
#'  | notes                 | character |
#'  | date_signed           | character |
#'  | sort_order            | integer   |
#'  | player_id             | integer   |
#'  | player_full_name      | character |
#'  | player_link           | character |
#'  | original_team_id      | integer   |
#'  | original_team_name    | character |
#'  | original_team_link    | character |
#'  | position_code         | character |
#'  | position_name         | character |
#'  | position_type         | character |
#'  | position_abbreviation | character |
#'  | new_team_id           | integer   |
#'  | new_team_name         | character |
#'  | new_team_link         | character |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_people_free_agents(season = 2018))
#' }
mlb_people_free_agents <- function(season = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/people/freeAgents"))
  query_params <- list(
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>%
        mlb_api_call()
      free_agents <- jsonlite::fromJSON(jsonlite::toJSON(resp$freeAgents), flatten = TRUE) %>%
        janitor::clean_names() %>%
        make_baseballr_data("MLB People - Free Agents data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(free_agents)
}

#' @title **Find Information About MLB Free Agents**
#' @param season Season preceding free agency
#' @return Returns a tibble with the following columns:
#'
#'  |col_name               |types      |description                                                  |
#'  |:----------------------|:----------|:------------------------------------------------------------|
#'  | date_declared         | character |Date the player declared free agency (YYYY-MM-DD).           |
#'  | notes                 | character |Notes on the signing (e.g. 'One-year contract').            |
#'  | date_signed           | character |Date the player signed a new contract (YYYY-MM-DD).          |
#'  | sort_order            | integer   |Display sort order for the free agent record.                |
#'  | player_id             | integer   |MLB player id of the free agent.                             |
#'  | player_full_name      | character |Free agent full name.                                        |
#'  | player_link           | character |API relative link to the player.                             |
#'  | original_team_id      | integer   |Team id the player left.                                     |
#'  | original_team_name    | character |Name of the team the player left.                            |
#'  | original_team_link    | character |API relative link to the original team.                      |
#'  | position_code         | character |Player position code.                                        |
#'  | position_name         | character |Player position name (e.g. 'Relief Pitcher').               |
#'  | position_type         | character |Player position type (e.g. 'Pitcher').                      |
#'  | position_abbreviation | character |Player position abbreviation (e.g. 'RP').                   |
#'  | new_team_id           | integer   |Team id the player signed with.                              |
#'  | new_team_name         | character |Name of the team the player signed with.                     |
#'  | new_team_link         | character |API relative link to the new team.                           |
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
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  free_agents <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |>
        mlb_api_call()
      free_agents <- jsonlite::fromJSON(jsonlite::toJSON(resp$freeAgents), flatten = TRUE) |>
        janitor::clean_names() |>
        make_baseballr_data("MLB People - Free Agents data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(free_agents)
}

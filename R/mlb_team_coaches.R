#' @title **MLB Team Coaches**
#' @param team_id Team ID to return team coach information for.
#' @param date Date to return team coach information for.
#' @param season Year to return team coach information for. 
#' 
#' @return Returns a tibble with the following columns
#'    |col_name         |types     |
#'    |:----------------|:---------|
#'    |jersey_number    |character |
#'    |job              |character |
#'    |job_id           |character |
#'    |title            |character |
#'    |person_id        |integer   |
#'    |person_full_name |character |
#'    |person_link      |character |
#' @export
#' @examples \donttest{
#'   try(mlb_team_coaches(team_id = 137, season = 2021))
#' }
mlb_team_coaches <- function(team_id = NULL,
                             date = NULL,
                             season = NULL){
  
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}/coaches"))
  query_params <- list(
    date = date,
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      team_coaches <- jsonlite::fromJSON(jsonlite::toJSON(resp[['roster']]), flatten = TRUE)  
      team_coaches$season <- NULL
      team_coaches <- team_coaches %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>%
        make_baseballr_data("MLB Team Coaches data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  
  return(team_coaches)
}


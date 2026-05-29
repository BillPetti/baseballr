#' @title **MLB Team Personnel**
#' @param team_id Team ID to return team coach information for.
#' @param date Date to return team coach information for.
#' 
#' @return Returns a tibble with the following columns
#'
#'    |col_name         |types     |description                          |
#'    |:----------------|:---------|:------------------------------------|
#'    |jersey_number    |character |Personnel uniform number.            |
#'    |job              |character |Job name (e.g., Special Assistant).  |
#'    |job_id           |character |Job code identifier.                 |
#'    |title            |character |Full personnel title.                |
#'    |person_id        |integer   |Personnel MLBAM person ID.           |
#'    |person_full_name |character |Personnel full name.                 |
#'    |person_link      |character |API link to the person.              |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_team_personnel(team_id = 137, date = "08/28/2016"))
#' }
mlb_team_personnel <- function(team_id = NULL,
                               date = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}/personnel"))
  query_params <- list(
    date = date
  )
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  team_personnel <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      team_personnel <- jsonlite::fromJSON(jsonlite::toJSON(resp[['roster']]), flatten = TRUE)  
      team_personnel$season <- NULL
      team_personnel <- team_personnel |> 
        janitor::clean_names() |> 
        as.data.frame() |>
        make_baseballr_data("MLB Team Personnel data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(team_personnel)
}


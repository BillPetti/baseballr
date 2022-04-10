#' @title **MLB Jobs Official Scorers** 
#' @param sport_id Return information for a given sport_id. 
#' @param date Return information for a given date. 
#' @return Returns a tibble with the following columns
#'  |col_name         |types     |
#'  |:----------------|:---------|
#'  |jersey_number    |character |
#'  |job              |character |
#'  |job_code         |character |
#'  |title            |character |
#'  |person_id        |integer   |
#'  |person_full_name |character |
#'  |person_link      |character |
#' @export
#' @examples \donttest{
#'   try(mlb_jobs_official_scorers(sport_id=1))
#' }
mlb_jobs_official_scorers <- function(
                     sport_id = NULL,
                     date = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/jobs/officialScorers")
  query_params <- list(
    sportId = sport_id,
    date = date
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  jobs <- jsonlite::fromJSON(jsonlite::toJSON(resp$roster), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    as.data.frame() %>% 
    dplyr::rename(
      job_code = .data$job_id)
  
  return(jobs)
}


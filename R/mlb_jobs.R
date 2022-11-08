#' @title **MLB Jobs** 
#' @param job_type Return information for a given job_type. See ```mlb_job_types()```
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
#'   try(mlb_jobs(job_type='UMPR'))
#' }
mlb_jobs <- function(job_type='UMPR',
                     sport_id = NULL,
                     date = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/jobs")
  query_params <- list(
    jobType = job_type,
    sportId = sport_id,
    date = date
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      jobs <- jsonlite::fromJSON(jsonlite::toJSON(resp$roster), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          "job_code" = "job_id") %>%
        make_baseballr_data("MLB Jobs data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  return(jobs)
}


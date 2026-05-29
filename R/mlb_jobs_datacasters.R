#' @title **MLB Jobs Datacasters** 
#' @param sport_id Return information for a given sport_id. 
#' @param date Return information for a given date. 
#' @return Returns a tibble with the following columns
#'
#'  |col_name         |types     |description                                                          |
#'  |:----------------|:---------|:--------------------------------------------------------------------|
#'  |jersey_number    |character |Jersey number (typically blank for datacasters).                     |
#'  |job              |character |Job title (e.g. 'Stringer').                                        |
#'  |job_code         |character |Four-letter job type code (e.g. 'MSTR').                            |
#'  |title            |character |Specific role title for the assignment.                              |
#'  |person_id        |integer   |MLB person id for the datacaster.                                    |
#'  |person_full_name |character |Full name of the datacaster.                                         |
#'  |person_link      |character |API relative link to the person.                                     |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_jobs_datacasters(sport_id=1))
#' }
mlb_jobs_datacasters <- function(
  sport_id = NULL,
  date = NULL){
  
  jobs <- NULL
  tryCatch(
    expr = {
      mlb_endpoint <- mlb_stats_endpoint("v1/jobs/datacasters")
      query_params <- list(
        sportId = sport_id,
        date = date
      )
      
      mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
      
      resp <- mlb_endpoint |> 
        mlb_api_call()
      jobs <- jsonlite::fromJSON(jsonlite::toJSON(resp$roster), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "job_code" = "job_id") |>
        make_baseballr_data("MLB Jobs Datacasters data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(jobs)
}


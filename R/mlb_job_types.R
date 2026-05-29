#' @title **MLB Job Types** 
#' @return Returns a tibble with the following columns
#'  |col_name   |types     |description                                                          |
#'  |:----------|:---------|:--------------------------------------------------------------------|
#'  |job_code   |character |Four-letter job type code (e.g. 'UMPR', 'UDIR').                     |
#'  |job        |character |Job title (e.g. 'Umpire', 'Director of Instant Replay').            |
#'  |sort_order |integer   |Display sort order for the job type.                                 |
#' @export
#' @examples \donttest{
#'   try(mlb_job_types())
#' }
mlb_job_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/jobTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  job_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      job_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "job_code" = "code") |>
        make_baseballr_data("MLB Job Types data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(job_types)
}


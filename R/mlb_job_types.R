#' @title **MLB Job Types** 
#' @return Returns a tibble with the following columns
#'  |col_name   |types     |
#'  |:----------|:---------|
#'  |job_code   |character |
#'  |job        |character |
#'  |sort_order |integer   |
#' @export
#' @examples \donttest{
#'   mlb_job_types()
#' }
mlb_job_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/jobTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  job_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    as.data.frame() %>% 
    dplyr::rename(
      job_code = .data$code)
  
  return(job_types)
}


#' @title **MLB Job Types** 
#' @return Returns a tibble with the following columns
#'  |col_name   |types     |
#'  |:----------|:---------|
#'  |job_code   |character |
#'  |job        |character |
#'  |sort_order |integer   |
#' @export
#' @examples \donttest{
#'   try(mlb_job_types())
#' }
mlb_job_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/jobTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      job_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          job_code = .data$code) %>%
        make_baseballr_data("MLB Job Types data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(job_types)
}


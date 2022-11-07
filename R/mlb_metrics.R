#' @title **MLB Metrics** 
#' @return Returns a tibble with the following columns
#'  |col_name    |types     |
#'  |:-----------|:---------|
#'  |metric_name |character |
#'  |metric_id   |integer   |
#'  |stat_group  |character |
#'  |metric_unit |character |
#' @export
#' @examples \donttest{
#'   try(mlb_metrics())
#' }
mlb_metrics <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/metrics")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      metrics <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          metric_name = .data$name,
          stat_group = .data$group,
          metric_unit = .data$unit) %>%
        make_baseballr_data("MLB Metrics data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  return(metrics)
}


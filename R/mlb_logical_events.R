#' @title **MLB Logical Events** 
#' @return Returns a tibble with the following columns
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |event_code    |character |
#' @export
#' @examples \donttest{
#'   mlb_logical_events()
#' }
mlb_logical_events <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/logicalEvents")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  logical_events <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    as.data.frame() %>% 
    dplyr::rename(
      event_code = .data$code)
  
  return(logical_events)
}


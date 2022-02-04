#' @title **MLB Runner Detail Types** 
#' @return Returns a data frame with the following columns
#'  |col_name  |types     |
#'  |:---------|:---------|
#'  |stat_name |character |
#' @export
#' @examples \donttest{
#'   mlb_runner_detail_types()
#' }
mlb_runner_detail_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/runnerDetailTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  runner_detail_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    as.data.frame()
  return(runner_detail_types)
}


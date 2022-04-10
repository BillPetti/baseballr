#' @title **MLB Hit Trajectories** 
#' @return Returns a tibble with the following columns
#'  |col_name                   |types     |
#'  |:--------------------------|:---------|
#'  |hit_trajectory_code        |character |
#'  |hit_trajectory_description |character |
#' @export
#' @examples \donttest{
#'   try(mlb_hit_trajectories())
#' }
mlb_hit_trajectories <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/hitTrajectories")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  hit_trajectories <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    as.data.frame() %>% 
    dplyr::rename(
      hit_trajectory_code = .data$code,
      hit_trajectory_description = .data$description)
  
  return(hit_trajectories)
}


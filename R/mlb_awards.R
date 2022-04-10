#' @title **MLB Awards** 
#' @return Returns a data frame with the following columns
#'  |col_name          |types     |
#'  |:-----------------|:---------|
#'  |award_id          |character |
#'  |award_name        |character |
#'  |award_description |character |
#'  |sort_order        |integer   |
#'  |notes             |character |
#'  |sport_id          |integer   |
#'  |sport_link        |character |
#'  |league_id         |integer   |
#'  |league_link       |character |
#' @export
#' @examples \donttest{
#'   try(mlb_awards())
#' }
mlb_awards <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/awards")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  awards <- jsonlite::fromJSON(jsonlite::toJSON(resp$awards), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    dplyr::rename(
      award_id = .data$id,
      award_name = .data$name,
      award_description = .data$description)
  
  return(awards)
}
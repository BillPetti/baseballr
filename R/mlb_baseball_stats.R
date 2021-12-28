#' @title **MLB Baseball Stats** 
#' @return Returns a data frame with the following columns
#'  |col_name          |types     |
#'  |:-----------------|:---------|
#'  |stat_name         |character |
#'  |stat_lookup_param |character |
#'  |is_counting       |logical   |
#'  |stat_label        |character |
#'  |stat_group        |character |
#' @export
#' @examples \donttest{
#'   mlb_baseball_stats()
#' }
mlb_baseball_stats <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/baseballStats")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  baseball_stats <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    tidyr::unnest(.data$stat_groups, names_sep = "_")
  baseball_stats <- baseball_stats[,1:5] %>% 
    dplyr::rename(
      stat_name = .data$name,
      stat_lookup_param = .data$lookup_param,
      stat_label = .data$label,
      stat_group = .data$stat_groups_displayName
    )
  return(baseball_stats)
}


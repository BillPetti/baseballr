#' @title **MLB Positions** 
#' @return Returns a tibble with the following columns
#'  |col_name              |types     |
#'  |:---------------------|:---------|
#'  |position_short_name   |character |
#'  |position_full_name    |character |
#'  |position_abbreviation |character |
#'  |position_code         |character |
#'  |position_type         |character |
#'  |position_formal_name  |character |
#'  |game_position         |logical   |
#'  |pitcher               |logical   |
#'  |fielder               |logical   |
#'  |outfield              |logical   |
#'  |position_display_name |character |
#' @export
#' @examples \donttest{
#'   mlb_positions()
#' }
mlb_positions <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/positions")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  positions <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    as.data.frame() %>% 
    dplyr::rename(
      position_short_name = .data$short_name,
      position_full_name = .data$full_name,
      position_abbreviation = .data$abbrev,
      position_code = .data$code,
      position_type = .data$type,
      position_formal_name = .data$formal_name,
      position_display_name = .data$display_name
    )
  
  return(positions)
}


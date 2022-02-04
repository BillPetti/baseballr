#' @title **MLB Game Status Codes** 
#' @return Returns a data frame with the following columns
#'  |col_name            |types     |
#'  |:-------------------|:---------|
#'  |abstract_game_state |character |
#'  |coded_game_state    |character |
#'  |detailed_state      |character |
#'  |status_code         |character |
#'  |reason              |character |
#'  |abstract_game_code  |character |
#' @export
#' @examples \donttest{
#'   mlb_game_status_codes()
#' }
mlb_game_status_codes <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/gameStatus")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  game_status_codes <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    as.data.frame()
  return(game_status_codes)
}


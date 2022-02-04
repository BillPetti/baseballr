#' @title **MLB Sky (Weather) Codes** 
#' @return Returns a tibble with the following columns
#'  |col_name            |types     |
#'  |:-------------------|:---------|
#'  |sky_code            |character |
#'  |sky_description     |character |
#' @export
#' @examples \donttest{
#'   mlb_sky()
#' }
mlb_sky <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/sky")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  sky <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    as.data.frame() %>% 
    dplyr::rename(
      sky_code = .data$code,
      sky_description = .data$description
    )
  
  return(sky)
}


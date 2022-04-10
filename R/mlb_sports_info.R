#' @title **MLB Sport IDs Information**
#' @param sport_id The sport_id to return information for.
#' @return Returns a tibble with the following columns
#'  |col_name           |types     |
#'  |:------------------|:---------|
#'  |sport_id           |integer   |
#'  |sport_code         |character |
#'  |sport_link         |character |
#'  |sport_name         |character |
#'  |sport_abbreviation |character |
#'  |sort_order         |integer   |
#'  |active_status      |logical   |
#'  
#' @export
#' @examples \donttest{
#'   try(mlb_sports_info(sport_id = 1))
#' }
mlb_sports_info <- function(sport_id = 1){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/sports/{sport_id}"))
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  
  sports <- resp$sports %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    as.data.frame() %>%  
    janitor::clean_names() %>% 
    dplyr::rename(
      sport_id = .data$id,
      sport_code = .data$code,
      sport_link = .data$link,
      sport_name = .data$name,
      sport_abbreviation = .data$abbreviation
    )
  
  return(sports)
}


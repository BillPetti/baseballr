#' @title **MLB Situation Codes** 
#' @return Returns a tibble with the following columns
#'  |col_name                   |types     |
#'  |:--------------------------|:---------|
#'  |situation_code             |character |
#'  |sort_order                 |integer   |
#'  |navigation_menu            |character |
#'  |situation_code_description |character |
#'  |team                       |logical   |
#'  |batting                    |logical   |
#'  |fielding                   |logical   |
#'  |pitching                   |logical   |
#' @export
#' @examples \donttest{
#'   try(mlb_situation_codes())
#' }
mlb_situation_codes <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/situationCodes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      situation_codes <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          situation_code = .data$code,
          situation_code_description = .data$description
        ) %>%
        make_baseballr_data("MLB Situation Codes data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  return(situation_codes)
}


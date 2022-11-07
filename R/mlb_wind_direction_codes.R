#' @title **MLB Wind Direction Codes** 
#' @return Returns a tibble with the following columns
#'  |col_name                   |types     |
#'  |:--------------------------|:---------|
#'  |wind_direction_code        |character |
#'  |wind_direction_description |character |
#' @export
#' @examples \donttest{
#'   try(mlb_wind_direction_codes())
#' }
mlb_wind_direction_codes <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/windDirection")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      wind_direction_codes <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          wind_direction_code = .data$code,
          wind_direction_description = .data$description) %>%
        make_baseballr_data("MLB Wind Direction Codes data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  return(wind_direction_codes)
}


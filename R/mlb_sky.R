#' @title **MLB Sky (Weather) Codes** 
#' @return Returns a tibble with the following columns
#'  |col_name            |types     |
#'  |:-------------------|:---------|
#'  |sky_code            |character |
#'  |sky_description     |character |
#' @export
#' @examples \donttest{
#'   try(mlb_sky())
#' }
mlb_sky <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/sky")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      sky <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          "sky_code" = "code",
          "sky_description" = "description") %>%
        make_baseballr_data("MLB Sky (Weather) Codes data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(sky)
}


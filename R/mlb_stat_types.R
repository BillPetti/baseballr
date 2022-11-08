#' @title **MLB Stat Types** 
#' @return Returns a tibble with the following columns
#'  |col_name              |types     |
#'  |:---------------------|:---------|
#'  |stat_type_name        |character |
#' @export
#' @examples \donttest{
#'   try(mlb_stat_types())
#' }
mlb_stat_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/statTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      stat_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          "stat_type_name" = "display_name") %>%
        make_baseballr_data("MLB Stat Types data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(stat_types)
}


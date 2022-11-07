#' @title **MLB API Language Options** 
#' @return Returns a tibble with the following columns
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |language_name |character |
#'  |language_code |character |
#'  |locale        |character |
#' @export
#' @examples \donttest{
#'   try(mlb_languages())
#' }
mlb_languages <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/languages")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      languages <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          language_name = .data$name) %>%
        make_baseballr_data("MLB Languages data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  return(languages)
}


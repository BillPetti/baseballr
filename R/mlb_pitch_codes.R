#' @title **MLB Pitch Codes** 
#' @return Returns a tibble with the following columns
#'  |col_name           |types     |
#'  |:------------------|:---------|
#'  |pitch_code         |character |
#'  |pitch_description  |character |
#' @export
#' @examples \donttest{
#'   try(mlb_pitch_codes())
#' }
mlb_pitch_codes <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/pitchCodes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      pitch_codes <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          pitch_code = .data$code,
          pitch_description = .data$description) %>%
        make_baseballr_data("MLB Pitch Codes data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(pitch_codes)
}


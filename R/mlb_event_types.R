#' @title **MLB Event Types** 
#' @return Returns a data frame with the following columns
#'  |col_name           |types     |
#'  |:------------------|:---------|
#'  |plate_appearance   |logical   |
#'  |hit                |logical   |
#'  |event_code         |character |
#'  |base_running_event |logical   |
#'  |event_description  |character |
#' @export
#' @examples \donttest{
#'   try(mlb_event_types())
#' }
mlb_event_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/eventTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      event_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        dplyr::rename(
          event_code = .data$code,
          event_description = .data$description) %>%
        make_baseballr_data("MLB Event Types data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(event_types)
}


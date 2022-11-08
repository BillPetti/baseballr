#' @title **MLB Schedule Event Types** 
#' @return Returns a tibble with the following columns
#'  |col_name                 |types     |
#'  |:------------------------|:---------|
#'  |schedule_event_type_code |character |
#'  |schedule_event_type_name |character |
#' @export
#' @examples \donttest{
#'   try(mlb_schedule_event_types())
#' }
mlb_schedule_event_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/scheduleEventTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      schedule_event_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          "schedule_event_type_code" = "code",
          "schedule_event_type_name" = "name"
        ) %>%
        make_baseballr_data("MLB Schedule Event Types data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  return(schedule_event_types)
}


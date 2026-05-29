#' @title **MLB Schedule Event Types** 
#' @return Returns a tibble with the following columns
#'  |col_name                 |types     |description                                          |
#'  |:------------------------|:---------|:----------------------------------------------------|
#'  |schedule_event_type_code |character |Short code for the schedule event type.              |
#'  |schedule_event_type_name |character |Name of the event type (e.g. 'All-Star Weekend Event').|
#' @export
#' @examples \donttest{
#'   try(mlb_schedule_event_types())
#' }
mlb_schedule_event_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/scheduleEventTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  schedule_event_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      schedule_event_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "schedule_event_type_code" = "code",
          "schedule_event_type_name" = "name"
        ) |>
        make_baseballr_data("MLB Schedule Event Types data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(schedule_event_types)
}


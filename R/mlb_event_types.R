#' @title **MLB Event Types** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name           |types     |description                                       |
#'  |:------------------|:---------|:-------------------------------------------------|
#'  |plate_appearance   |logical   |Whether the event counts as a plate appearance.   |
#'  |hit                |logical   |Whether the event is a hit.                       |
#'  |event_code         |character |Event type code.                                  |
#'  |base_running_event |logical   |Whether the event is a base-running event.        |
#'  |event_description  |character |Human-readable event description.                 |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_event_types())
#' }
mlb_event_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/eventTypes")
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  event_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      event_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        dplyr::rename(
          "event_code" = "code",
          "event_description" = "description") |>
        make_baseballr_data("MLB Event Types data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(event_types)
}


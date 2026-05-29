#' @title **MLB Logical Events** 
#' @return Returns a tibble with the following columns
#'  |col_name      |types     |description                                                          |
#'  |:-------------|:---------|:--------------------------------------------------------------------|
#'  |event_code    |character |Logical event code used by the MLB Gameday feed (e.g. 'newBatter').  |
#' @export
#' @examples \donttest{
#'   try(mlb_logical_events())
#' }
mlb_logical_events <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/logicalEvents")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  logical_events <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      logical_events <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "event_code" = "code") |>
        make_baseballr_data("MLB Logical Events data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(logical_events)
}


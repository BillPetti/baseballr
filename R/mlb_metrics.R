#' @title **MLB Metrics** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name    |types     |description                                                          |
#'  |:-----------|:---------|:--------------------------------------------------------------------|
#'  |metric_name |character |Metric name (e.g. 'releaseSpinRate').                               |
#'  |metric_id   |integer   |Numeric metric identifier.                                          |
#'  |stat_group  |character |Stat group the metric belongs to (e.g. 'pitching').                 |
#'  |metric_unit |character |Unit of measure for the metric (e.g. 'RPM').                        |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_metrics())
#' }
mlb_metrics <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/metrics")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  metrics <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      metrics <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "metric_name" = "name",
          "stat_group" = "group",
          "metric_unit" = "unit") |>
        make_baseballr_data("MLB Metrics data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(metrics)
}


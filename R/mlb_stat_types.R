#' @title **MLB Stat Types** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name              |types     |description                                |
#'  |:---------------------|:---------|:------------------------------------------|
#'  |stat_type_name        |character |Stat type display name (e.g., projected).  |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_stat_types())
#' }
mlb_stat_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/statTypes")
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  stat_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      stat_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "stat_type_name" = "display_name") |>
        make_baseballr_data("MLB Stat Types data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(stat_types)
}


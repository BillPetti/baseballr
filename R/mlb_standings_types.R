#' @title **MLB Standings Types** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name                   |types     |description                                  |
#'  |:--------------------------|:---------|:--------------------------------------------|
#'  |standings_type_name        |character |Standings type identifier (e.g., wildCard).  |
#'  |standings_type_description |character |Human-readable description of the type.      |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_standings_types())
#' }
mlb_standings_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/standingsTypes")
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  standings_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      standings_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "standings_type_name" = "name",
          "standings_type_description" = "description") |>
        make_baseballr_data("MLB Standings Types data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(standings_types)
}


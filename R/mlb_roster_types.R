#' @title **MLB Roster Types** 
#' @return Returns a tibble with the following columns
#'  |col_name                |types     |description                                            |
#'  |:-----------------------|:---------|:------------------------------------------------------|
#'  |roster_type_description |character |Description of the roster type.                        |
#'  |roster_type_lookup_name |character |Lookup name for the roster type.                       |
#'  |roster_type_parameter   |character |Parameter value to pass as `roster_type` in API calls.|
#' @export
#' @examples \donttest{
#'   try(mlb_roster_types())
#' }
mlb_roster_types <- function(){
  mlb_endpoint <- mlb_stats_endpoint("v1/rosterTypes")
  
  roster_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      roster_types <- resp |> 
        janitor::clean_names() |> 
        dplyr::rename(
          "roster_type_description" = "description",
          "roster_type_lookup_name" = "lookup_name",
          "roster_type_parameter" = "parameter") |>
        make_baseballr_data("MLB Roster Types data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(roster_types)
}

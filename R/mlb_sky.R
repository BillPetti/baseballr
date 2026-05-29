#' @title **MLB Sky (Weather) Codes** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name        |types     |description                                       |
#'  |:---------------|:---------|:-------------------------------------------------|
#'  |sky_code        |character |Code for the sky/weather condition.               |
#'  |sky_description |character |Description of the sky condition (e.g. 'Clear').  |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_sky())
#' }
mlb_sky <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/sky")
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  sky <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      sky <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "sky_code" = "code",
          "sky_description" = "description") |>
        make_baseballr_data("MLB Sky (Weather) Codes data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(sky)
}


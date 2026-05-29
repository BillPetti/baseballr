#' @title **MLB Pitch Types** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name               |types     |description                                       |
#'  |:----------------------|:---------|:-------------------------------------------------|
#'  |pitch_type_code        |character |Short code identifying the pitch type (e.g. 'FA').|
#'  |pitch_type_description |character |Full name of the pitch type (e.g. 'Fastball').    |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_pitch_types())
#' }
mlb_pitch_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/pitchTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  pitch_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      pitch_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "pitch_type_code" = "code",
          "pitch_type_description" = "description") |>
        make_baseballr_data("MLB Pitch Types data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(pitch_types)
}


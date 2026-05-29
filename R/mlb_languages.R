#' @title **MLB API Language Options** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name      |types     |description                                                          |
#'  |:-------------|:---------|:--------------------------------------------------------------------|
#'  |language_id   |integer   |Numeric language identifier.                                         |
#'  |language_code |character |Short language code (e.g. 'en', 'xe').                              |
#'  |language_name |character |Language display name (e.g. 'English', 'Elias').                    |
#'  |locale        |character |Locale string for the language (e.g. 'en_US').                      |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_languages())
#' }
mlb_languages <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/languages")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  languages <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      languages <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "language_name" = "name") |>
        make_baseballr_data("MLB Languages data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(languages)
}


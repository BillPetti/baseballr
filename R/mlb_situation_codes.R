#' @title **MLB Situation Codes** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name                   |types     |description                                       |
#'  |:--------------------------|:---------|:-------------------------------------------------|
#'  |situation_code             |character |Code identifying the game situation.              |
#'  |sort_order                 |integer   |Display sort order for the situation code.        |
#'  |navigation_menu            |character |Navigation menu grouping (e.g. 'Game').           |
#'  |situation_code_description |character |Description of the situation (e.g. 'Home Games'). |
#'  |team                       |logical   |Whether the situation applies to team stats.      |
#'  |batting                    |logical   |Whether the situation applies to batting stats.   |
#'  |fielding                   |logical   |Whether the situation applies to fielding stats.  |
#'  |pitching                   |logical   |Whether the situation applies to pitching stats.  |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_situation_codes())
#' }
mlb_situation_codes <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/situationCodes")
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  situation_codes <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      situation_codes <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "situation_code" = "code",
          "situation_code_description" = "description"
        ) |>
        make_baseballr_data("MLB Situation Codes data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(situation_codes)
}


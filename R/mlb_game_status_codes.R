#' @title **MLB Game Status Codes** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name            |types     |description                                                              |
#'  |:-------------------|:---------|:------------------------------------------------------------------------|
#'  |abstract_game_state |character |Abstract game state grouping (e.g. 'Preview', 'Live', 'Final').           |
#'  |coded_game_state    |character |Single-letter coded game state (e.g. 'S', 'P', 'I', 'F').                |
#'  |detailed_state      |character |Detailed status description (e.g. 'Scheduled', 'Pre-Game', 'In Progress').|
#'  |status_code         |character |Status code identifier (e.g. 'S', 'P', 'I', 'F').                        |
#'  |reason              |character |Reason text for the status when applicable (e.g. delay/postponement).     |
#'  |abstract_game_code  |character |Single-letter abstract game code (e.g. 'P', 'L', 'F').                   |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_game_status_codes())
#' }
mlb_game_status_codes <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/gameStatus")
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  game_status_codes <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      game_status_codes <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |>
        make_baseballr_data("MLB Game Status Codes data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(game_status_codes)
}


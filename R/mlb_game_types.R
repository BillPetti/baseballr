#' @title **MLB Game Types** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name              |types     |description                                                          |
#'  |:---------------------|:---------|:--------------------------------------------------------------------|
#'  |game_type_id          |character |Single-letter game type code (e.g. 'S', 'R', 'P', 'W').              |
#'  |game_type_description |character |Game type description (e.g. 'Spring Training', 'Regular Season').    |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_game_types())
#' }
mlb_game_types <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/gameTypes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  game_types <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      game_types <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "game_type_id" = "id",
          "game_type_description" = "description") |>
        make_baseballr_data("MLB Game Types data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(game_types)
}


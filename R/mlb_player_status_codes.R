#' @title **MLB Player Status Codes** 
#' @return Returns a tibble with the following columns
#'  |col_name                  |types     |description                                       |
#'  |:-------------------------|:---------|:-------------------------------------------------|
#'  |player_status_code        |character |Short code for the player status (e.g. 'A').      |
#'  |player_status_description |character |Description of the player status (e.g. 'Active'). |
#' @export
#' @examples \donttest{
#'   try(mlb_player_status_codes())
#' }
mlb_player_status_codes <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/playerStatusCodes")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  player_status_codes <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      player_status_codes <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "player_status_code" = "code",
          "player_status_description" = "description") |>
        make_baseballr_data("MLB Player Status Codes data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(player_status_codes)
}


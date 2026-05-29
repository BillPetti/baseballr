#' @title **MLB Positions** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name              |types     |description                                          |
#'  |:---------------------|:---------|:----------------------------------------------------|
#'  |position_short_name   |character |Short position name (e.g. 'Pitcher').                |
#'  |position_full_name    |character |Full position name.                                  |
#'  |position_abbreviation |character |Position abbreviation (e.g. 'P', 'SS').              |
#'  |position_code         |character |Numeric scorekeeping position code.                  |
#'  |position_type         |character |Position category (e.g. 'Pitcher', 'Infielder').     |
#'  |position_formal_name  |character |Formal position name.                                |
#'  |position_display_name |character |Display name for the position.                       |
#'  |outfield              |logical   |Whether the position is an outfield position.        |
#'  |game_position         |logical   |Whether it is an on-field game position.             |
#'  |pitcher               |logical   |Whether the position is a pitcher.                   |
#'  |fielder               |logical   |Whether the position is a fielder.                   |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_positions())
#' }
mlb_positions <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/positions")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  positions <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      positions <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "position_short_name" = "short_name",
          "position_full_name" = "full_name",
          "position_abbreviation" = "abbrev",
          "position_code" = "code",
          "position_type" = "type",
          "position_formal_name" = "formal_name",
          "position_display_name" = "display_name") |> 
        dplyr::select(c(
          "position_short_name", "position_full_name",
          "position_abbreviation", "position_code", 
          "position_type", "position_formal_name",
          "position_display_name", "outfield", "game_position",
          "pitcher", "fielder")) |>
        make_baseballr_data("MLB Positions data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(positions)
}


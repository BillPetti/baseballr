#' @title **MLB Sport IDs Information**
#' @param sport_id The sport_id to return information for.
#' @return Returns a tibble with the following columns
#'
#'  |col_name           |types     |description                                       |
#'  |:------------------|:---------|:-------------------------------------------------|
#'  |sport_id           |integer   |MLBAM sport (level) identifier.                   |
#'  |sport_code         |character |Short sport code (e.g. 'mlb', 'aaa').             |
#'  |sport_link         |character |API link to the sport resource.                  |
#'  |sport_name         |character |Full sport/level name.                            |
#'  |sport_abbreviation |character |Sport abbreviation (e.g. 'MLB').                  |
#'  |sort_order         |integer   |Display sort order for the sport.                 |
#'  |active_status      |logical   |Whether the sport/level is active.                |
#'  
#' @export
#' @examples \donttest{
#'   try(mlb_sports_info(sport_id = 1))
#' }
mlb_sports_info <- function(sport_id = 1){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/sports/{sport_id}"))
  query_params <- list()
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  sports <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      
      sports <- resp$sports |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE) |> 
        as.data.frame() |>  
        janitor::clean_names() |> 
        dplyr::rename(
          "sport_id" = "id",
          "sport_code" = "code",
          "sport_link" = "link",
          "sport_name" = "name",
          "sport_abbreviation" = "abbreviation") |>
        make_baseballr_data("MLB Sports Info data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(sports)
}


#' @title **MLB Awards** 
#' @return Returns a tibble with the following columns
#'
#'  |col_name          |types     |description                                  |
#'  |:-----------------|:---------|:--------------------------------------------|
#'  |award_id          |character |Award identifier code.                       |
#'  |award_name        |character |Award name.                                  |
#'  |award_description |character |Award description.                           |
#'  |sort_order        |integer   |Display sort order for the award.            |
#'  |active            |logical   |Whether the award is currently active.       |
#'  |notes             |character |Additional notes about the award.            |
#'  |sport_id          |integer   |MLB sport ID associated with the award.      |
#'  |sport_link        |character |MLB Stats API relative sport link.           |
#'  |league_id         |integer   |MLB league ID associated with the award.     |
#'  |league_link       |character |MLB Stats API relative league link.          |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_awards())
#' }
mlb_awards <- function(){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/awards")
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  awards <- NULL
  tryCatch(
    expr = {
  resp <- mlb_endpoint |> 
    mlb_api_call()
  awards <- jsonlite::fromJSON(jsonlite::toJSON(resp$awards), flatten = TRUE)  |> 
    janitor::clean_names() |> 
    dplyr::rename(
      "award_id" = "id",
      "award_name" = "name",
      "award_description" = "description") |>
    make_baseballr_data("MLB Awards data from MLB.com",Sys.time())
  
    },
  error = function(e) {
    cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
  },
  finally = {
  }
  )
  return(awards)
}

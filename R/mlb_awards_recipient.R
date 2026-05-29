#' @title **MLB Award Recipients** 
#' @param award_id award_id to return a directory of players for a given award.
#' @param sport_id sport_id to return a directory of players for a given aware in a specific sport.
#' @param league_id league_id(s) to return a directory of players for a given award in a specific league. Format '103,104'
#' @param season Year(s) to return a directory of players for a given award in a given season.
#' @return Returns a tibble with the following columns
#'  |col_name                             |types     |description                                  |
#'  |:------------------------------------|:---------|:--------------------------------------------|
#'  |award_id                             |character |Award identifier code.                       |
#'  |award_name                           |character |Award name.                                  |
#'  |date                                 |character |Date the award was given (YYYY-MM-DD).       |
#'  |season                               |character |Season the award was given (YYYY).           |
#'  |votes                                |integer   |Number of votes received.                    |
#'  |notes                                |character |Additional notes about the recipient.        |
#'  |player_id                            |integer   |MLB player ID of the recipient.              |
#'  |player_link                          |character |MLB Stats API relative player link.          |
#'  |player_name_first_last               |character |Recipient name in first-last order.          |
#'  |player_primary_position_code         |character |Recipient primary fielding position code.    |
#'  |player_primary_position_name         |character |Recipient primary fielding position name.    |
#'  |player_primary_position_type         |character |Recipient primary position type.             |
#'  |player_primary_position_abbreviation |character |Recipient primary position abbreviation.     |
#'  |team_id                              |integer   |MLB team ID of the recipient.                |
#'  |team_link                            |character |MLB Stats API relative team link.            |
#' @export
#' @examples \donttest{
#'   try(mlb_awards_recipient(award_id = 'MLBHOF', season = 2020))
#' }
mlb_awards_recipient <- function(award_id = NULL, sport_id = NULL, league_id = NULL, season = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/awards/{award_id}/recipients"))
  query_params <- list(
    sportId = sport_id, 
    leagueId = league_id, 
    season = season
  )
  
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
          "award_name" = "name") |>
        make_baseballr_data("MLB Awards Recipient data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(awards)
}

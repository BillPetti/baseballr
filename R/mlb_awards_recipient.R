#' @title **MLB Award Recipients** 
#' @param award_id award_id to return a directory of players for a given award.
#' @param sport_id sport_id to return a directory of players for a given aware in a specific sport.
#' @param league_id league_id(s) to return a directory of players for a given award in a specific league. Format '103,104'
#' @param season Year(s) to return a directory of players for a given award in a given season.
#' @return Returns a data frame with the following columns
#'  |col_name                             |types     |
#'  |:------------------------------------|:---------|
#'  |award_id                             |character |
#'  |award_name                           |character |
#'  |date                                 |character |
#'  |season                               |character |
#'  |votes                                |integer   |
#'  |notes                                |character |
#'  |player_id                            |integer   |
#'  |player_link                          |character |
#'  |player_name_first_last               |character |
#'  |player_primary_position_code         |character |
#'  |player_primary_position_name         |character |
#'  |player_primary_position_type         |character |
#'  |player_primary_position_abbreviation |character |
#'  |team_id                              |integer   |
#'  |team_link                            |character |
#' @export
#' @examples \donttest{
#'   mlb_awards_recipient(award_id = 'MLBHOF', season = 2020)
#' }
mlb_awards_recipient <- function(award_id = NULL, sport_id = NULL, league_id = NULL, season = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/awards/{award_id}/recipients"))
  query_params <- list(
    sportId = sport_id, 
    leagueId = league_id, 
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  awards <- jsonlite::fromJSON(jsonlite::toJSON(resp$awards), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    dplyr::rename(
      award_id = .data$id,
      award_name = .data$name)
  
  return(awards)
}
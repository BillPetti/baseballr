#' @rdname mlb_game_pace
#' @title **Retrieve game pace metrics for major and minor league**
#' @param season Year for which to return information (*Required*).
#' @param team_ids The team_id(s) for which to return information.
#' @param league_ids The league_id(s) for which to return information.
#' @param sport_ids The sport_id(s) for which to return information.
#' @param game_type The game_type for which to return information.
#' @param venue_ids Venue directorial information based venue_id. 
#' @param org_type pace of game metrics based on team ('T'), league ('L') or sport('S')
#' @param start_date Date of first game for which you want data.
#' Format must be in MM/DD/YYYY format.
#' @param end_date Date of last game for which you want data.
#' Format must be in MM/DD/YYYY format.
#' @return Returns a data frame with the following columns
#'   |col_name                                            |types     |
#'   |:---------------------------------------------------|:---------|
#'   |hits_per9inn                                        |numeric   |
#'   |runs_per9inn                                        |numeric   |
#'   |pitches_per9inn                                     |numeric   |
#'   |plate_appearances_per9inn                           |numeric   |
#'   |hits_per_game                                       |numeric   |
#'   |runs_per_game                                       |numeric   |
#'   |innings_played_per_game                             |numeric   |
#'   |pitches_per_game                                    |numeric   |
#'   |pitchers_per_game                                   |numeric   |
#'   |plate_appearances_per_game                          |numeric   |
#'   |total_game_time                                     |character |
#'   |total_innings_played                                |integer   |
#'   |total_hits                                          |integer   |
#'   |total_runs                                          |integer   |
#'   |total_plate_appearances                             |integer   |
#'   |total_pitchers                                      |integer   |
#'   |total_pitches                                       |integer   |
#'   |total_games                                         |integer   |
#'   |total7inn_games                                     |integer   |
#'   |total9inn_games                                     |integer   |
#'   |total_extra_inn_games                               |integer   |
#'   |time_per_game                                       |character |
#'   |time_per_pitch                                      |character |
#'   |time_per_hit                                        |character |
#'   |time_per_run                                        |character |
#'   |time_per_plate_appearance                           |character |
#'   |time_per9inn                                        |character |
#'   |time_per77plate_appearances                         |character |
#'   |total_extra_inn_time                                |character |
#'   |time_per7inn_game                                   |character |
#'   |time_per7inn_game_without_extra_inn                 |character |
#'   |total7inn_games_scheduled                           |integer   |
#'   |total7inn_games_without_extra_inn                   |integer   |
#'   |total9inn_games_completed_early                     |integer   |
#'   |total9inn_games_without_extra_inn                   |integer   |
#'   |total9inn_games_scheduled                           |integer   |
#'   |hits_per_run                                        |numeric   |
#'   |pitches_per_pitcher                                 |numeric   |
#'   |season                                              |character |
#'   |sport_id                                            |integer   |
#'   |sport_code                                          |character |
#'   |sport_link                                          |character |
#'   |pr_portal_calculated_fields_total7inn_games         |integer   |
#'   |pr_portal_calculated_fields_total9inn_games         |integer   |
#'   |pr_portal_calculated_fields_total_extra_inn_games   |integer   |
#'   |pr_portal_calculated_fields_time_per7inn_game       |character |
#'   |pr_portal_calculated_fields_time_per9inn_game       |character |
#'   |pr_portal_calculated_fields_time_per_extra_inn_game |character |
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @export
#' @examples \donttest{
#'   try(mlb_game_pace(season = 2021, start_date = "09/14/2021", end_date = "09/16/2021"))
#' }

mlb_game_pace <- function(
  season, 
  league_ids = NULL, 
  sport_ids = NULL, 
  team_ids = NULL,
  game_type = NULL, 
  venue_ids = NULL,  
  org_type = NULL,
  start_date = NULL,
  end_date = NULL) {
  query_params <- list(
    season = season, 
    leagueId = league_ids,
    teamIds = team_ids,
    sportIds = sport_ids,
    gameType = game_type,
    venueIds = venue_ids, 
    orgType = org_type,
    startDate = start_date,
    endDate = end_date
  )
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/gamePace"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  resp <- mlb_endpoint %>% 
    mlb_api_call() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE)
  game_pace <- resp$sports %>% 
    as.data.frame() %>% 
    janitor::clean_names()
  
  return(game_pace)
}

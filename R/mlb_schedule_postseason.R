#' @title **Find game_pk values for professional baseball postseason games (major and minor leagues)**
#'
#' @param season The season for which you want to find game_pk values for MLB games
#' @param game_type game_type to return schedule information for all tied games in a particular game_type
#' @param series_number The Series number to return schedule information for all tied games in a particular series number
#' @param sport_id The sport_id to return schedule information for.
#' @param team_id The team_id to return schedule information for.
#' 
#'  |game_type_id |game_type_description      |
#'  |:------------|:--------------------------|
#'  |S            |Spring Training            |
#'  |R            |Regular Season             |
#'  |F            |Wild Card Game             |
#'  |D            |Division Series            |
#'  |L            |League Championship Series |
#'  |W            |World Series               |
#'  |C            |Championship               |
#'  |N            |Nineteenth Century Series  |
#'  |P            |Playoffs                   |
#'  |A            |All-Star Game              |
#'  |I            |Intrasquad                 |
#'  |E            |Exhibition                 |
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes game_pk values and additional
#' information for games scheduled or played
#'  |col_name                        |types     |
#'  |:-------------------------------|:---------|
#'  |date                            |character |
#'  |total_items                     |integer   |
#'  |total_events                    |integer   |
#'  |total_games                     |integer   |
#'  |total_games_in_progress         |integer   |
#'  |game_pk                         |integer   |
#'  |link                            |character |
#'  |game_type                       |character |
#'  |season                          |character |
#'  |game_date                       |character |
#'  |official_date                   |character |
#'  |is_tie                          |logical   |
#'  |is_featured_game                |logical   |
#'  |game_number                     |integer   |
#'  |public_facing                   |logical   |
#'  |double_header                   |character |
#'  |gameday_type                    |character |
#'  |tiebreaker                      |character |
#'  |calendar_event_id               |character |
#'  |season_display                  |character |
#'  |day_night                       |character |
#'  |description                     |character |
#'  |scheduled_innings               |integer   |
#'  |reverse_home_away_status        |logical   |
#'  |games_in_series                 |integer   |
#'  |series_game_number              |integer   |
#'  |series_description              |character |
#'  |record_source                   |character |
#'  |if_necessary                    |character |
#'  |if_necessary_description        |character |
#'  |status_abstract_game_state      |character |
#'  |status_coded_game_state         |character |
#'  |status_detailed_state           |character |
#'  |status_status_code              |character |
#'  |status_start_time_tbd           |logical   |
#'  |status_abstract_game_code       |character |
#'  |teams_away_score                |integer   |
#'  |teams_away_is_winner            |logical   |
#'  |teams_away_split_squad          |logical   |
#'  |teams_away_series_number        |integer   |
#'  |teams_away_league_record_wins   |integer   |
#'  |teams_away_league_record_losses |integer   |
#'  |teams_away_league_record_pct    |character |
#'  |teams_away_team_id              |integer   |
#'  |teams_away_team_name            |character |
#'  |teams_away_team_link            |character |
#'  |teams_home_score                |integer   |
#'  |teams_home_is_winner            |logical   |
#'  |teams_home_split_squad          |logical   |
#'  |teams_home_series_number        |integer   |
#'  |teams_home_league_record_wins   |integer   |
#'  |teams_home_league_record_losses |integer   |
#'  |teams_home_league_record_pct    |character |
#'  |teams_home_team_id              |integer   |
#'  |teams_home_team_name            |character |
#'  |teams_home_team_link            |character |
#'  |venue_id                        |integer   |
#'  |venue_name                      |character |
#'  |venue_link                      |character |
#'  |content_link                    |character |
#'  |inning_break_length             |integer   |
#'  |reschedule_date                 |character |
#'  |reschedule_game_date            |character |
#'  |status_reason                   |character |
#'  |rescheduled_from                |character |
#'  |rescheduled_from_date           |character |
#'  |is_default_game                 |logical   |
#'  |events                          |list      |
#'  
#' @export
#'
#' @examples \donttest{
#'   try(mlb_schedule_postseason(season = 2021))
#' }

mlb_schedule_postseason <- function(season = 2021,
                                    game_type = NULL,
                                    series_number = NULL,
                                    sport_id = 1,
                                    team_id = NULL){
  mlb_endpoint <- mlb_stats_endpoint("v1/schedule/postseason")
  query_params <- list(
    season = season, 
    gameTypes = game_type,
    seriesNumber = series_number,
    teamId = team_id,
    sportId = sport_id
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  
  games <- data.frame()
  tryCatch(
    expr = {
  resp <- mlb_endpoint %>% 
    mlb_api_call()

  games <- jsonlite::fromJSON(jsonlite::toJSON(resp$dates),flatten = TRUE) %>% 
    tidyr::unnest(.data$games) %>%
    as.data.frame() %>%
    janitor::clean_names()
  
    },
  error = function(e) {
    message(glue::glue("{Sys.time()}: Invalid arguments or no MLB postseason schedule data available!"))
  },
  warning = function(w) {
  },
  finally = {
  }
  )

  return(games)

}

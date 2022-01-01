#' @rdname mlb_pbp_changes
#' @title **Acquire time codes for Major and Minor League games**
#'
#' @param updated_since Updated since date time
#' @param sport_id Return division(s) for all divisions in a specific sport.
#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes time codes from the game_pk requested
#' 
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
#'  |inning_break_length             |integer   |
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
#'  |status_reason                   |character |
#'  |rescheduled_from                |character |
#'  |rescheduled_from_date           |character |
#'  |resumed_from                    |character |
#'  |resumed_from_date               |character |
#'  |events                          |list      |
#'  
#' @export
#' @examples \donttest{
#'   mlb_pbp_changes(updated_since = "2021-08-10T19:08:24.000004Z", sport_id = 1)
#' }

mlb_pbp_changes <- function(updated_since, sport_id) {
  
  query_params <- list( 
    sportId = sport_id,
    updatedSince = updated_since
  )
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/game/changes"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint,
                                   query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call() 
  
  changes <- resp$dates %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    tidyr::unnest(.data$games) %>%
    as.data.frame() %>%
    janitor::clean_names()
  
  
  return(changes)
}

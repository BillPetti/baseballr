#' @title **Find game_pk values for professional baseball games (major and minor leagues)**
#'
#' @param season The season for which you want to find game_pk values for MLB games
#' @param level_ids A numeric vector with ids for each level where game_pks are
#' desired. See below for a reference of level ids.
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
#'  |game_number                     |integer   |
#'  |public_facing                   |logical   |
#'  |double_header                   |character |
#'  |gameday_type                    |character |
#'  |tiebreaker                      |character |
#'  |calendar_event_id               |character |
#'  |season_display                  |character |
#'  |day_night                       |character |
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
#'  |status_reason                   |character |
#'  |status_abstract_game_code       |character |
#'  |teams_away_split_squad          |logical   |
#'  |teams_away_series_number        |integer   |
#'  |teams_away_league_record_wins   |integer   |
#'  |teams_away_league_record_losses |integer   |
#'  |teams_away_league_record_pct    |character |
#'  |teams_away_team_id              |integer   |
#'  |teams_away_team_name            |character |
#'  |teams_away_team_link            |character |
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
#'  |is_tie                          |logical   |
#'  |description                     |character |
#'  |teams_away_score                |integer   |
#'  |teams_away_is_winner            |logical   |
#'  |teams_home_score                |integer   |
#'  |teams_home_is_winner            |logical   |
#'  |reschedule_date                 |character |
#'  |reschedule_game_date            |character |
#'  |rescheduled_from                |character |
#'  |rescheduled_from_date           |character |
#'  |resume_date                     |character |
#'  |resume_game_date                |character |
#'  |resumed_from                    |character |
#'  |resumed_from_date               |character |
#'  |events                          |list      |
#' @section Level IDs:
#'
#' The following IDs can be passed to the level_ids argument:
#'
#' 1 = MLB \cr
#' 11 = Triple-A \cr
#' 12 = Doubl-A \cr
#' 13 = Class A Advanced \cr
#' 14 = Class A \cr
#' 15 = Class A Short Season \cr
#' 5442 = Rookie Advanced \cr
#' 16 = Rookie \cr
#' 17 = Winter League \cr
#' @export
#'
#' @examples \donttest{
#'   mlb_schedule(season = "2019")
#' }

mlb_schedule <- function(season = 2019, level_ids = '1'){

  api_call <- paste0("http://statsapi.mlb.com/api/v1/schedule?language=en",
                     "&sportId=", level_ids, 
                     "&season=", season)

  payload <- jsonlite::fromJSON(api_call, flatten = TRUE)

  games <- payload$dates %>% 
    tidyr::unnest(.data$games) %>%
    as.data.frame() %>%
    janitor::clean_names()

  return(games)

}

#' @title **Find game_pk values for professional baseball games (major and minor leagues) that are tied**
#'
#' @param season The season for which you want to find game_pk values for MLB games
#' @param game_type game_type to return schedule information for all tied games in a particular game_type
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
#' @return Returns a tibble that includes game_pk values and additional
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
#'  
#' @export
#'
#' @examples \donttest{
#'   try(mlb_schedule_games_tied(season = 2021))
#' }

mlb_schedule_games_tied <- function(season = 2021, game_type = 'S'){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/schedule/games/tied"))
  
  query_params <- list(
    season = season,
    gameTypes = game_type
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      
      games <- jsonlite::fromJSON(jsonlite::toJSON(resp$dates), flatten=TRUE) %>% 
        tidyr::unnest(.data$games) %>%
        as.data.frame() %>%
        janitor::clean_names() %>%
        make_baseballr_data("MLB Schedule - Games Tied data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(games)
  
}

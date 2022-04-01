#' @title **MLB Umpire Games** 
#' @param umpire_id Return information for a given umpire_id. 
#' @param season Return information for a given season. 
#' @return Returns a tibble with the following columns
#'   |col_name                        |types     |
#'   |:-------------------------------|:---------|
#'   |date                            |character |
#'   |total_items                     |integer   |
#'   |total_events                    |integer   |
#'   |total_games                     |integer   |
#'   |total_games_in_progress         |integer   |
#'   |game_pk                         |integer   |
#'   |link                            |character |
#'   |game_type                       |character |
#'   |season                          |character |
#'   |game_date                       |character |
#'   |official_date                   |character |
#'   |is_tie                          |logical   |
#'   |game_number                     |integer   |
#'   |public_facing                   |logical   |
#'   |double_header                   |character |
#'   |gameday_type                    |character |
#'   |tiebreaker                      |character |
#'   |calendar_event_id               |character |
#'   |season_display                  |character |
#'   |day_night                       |character |
#'   |scheduled_innings               |integer   |
#'   |reverse_home_away_status        |logical   |
#'   |games_in_series                 |integer   |
#'   |series_game_number              |integer   |
#'   |series_description              |character |
#'   |record_source                   |character |
#'   |if_necessary                    |character |
#'   |if_necessary_description        |character |
#'   |status_abstract_game_state      |character |
#'   |status_coded_game_state         |character |
#'   |status_detailed_state           |character |
#'   |status_status_code              |character |
#'   |status_start_time_tbd           |logical   |
#'   |status_abstract_game_code       |character |
#'   |teams_away_score                |integer   |
#'   |teams_away_is_winner            |logical   |
#'   |teams_away_split_squad          |logical   |
#'   |teams_away_series_number        |integer   |
#'   |teams_away_league_record_wins   |integer   |
#'   |teams_away_league_record_losses |integer   |
#'   |teams_away_league_record_pct    |character |
#'   |teams_away_team_id              |integer   |
#'   |teams_away_team_name            |character |
#'   |teams_away_team_link            |character |
#'   |teams_home_score                |integer   |
#'   |teams_home_is_winner            |logical   |
#'   |teams_home_split_squad          |logical   |
#'   |teams_home_series_number        |integer   |
#'   |teams_home_league_record_wins   |integer   |
#'   |teams_home_league_record_losses |integer   |
#'   |teams_home_league_record_pct    |character |
#'   |teams_home_team_id              |integer   |
#'   |teams_home_team_name            |character |
#'   |teams_home_team_link            |character |
#'   |venue_id                        |integer   |
#'   |venue_name                      |character |
#'   |venue_link                      |character |
#'   |content_link                    |character |
#'   |inning_break_length             |integer   |
#'   |status_reason                   |character |
#'   |description                     |character |
#'   |reschedule_date                 |character |
#'   |reschedule_game_date            |character |
#'   |rescheduled_from                |character |
#'   |rescheduled_from_date           |character |
#'   
#' @export
#' @examples \donttest{
#'   try(mlb_umpire_games(umpire_id = 608093))
#' }
mlb_umpire_games <- function(
                     umpire_id = NULL,
                     season = 2021){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/jobs/umpires/games/{umpire_id}"))
  query_params <- list(
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  games <- data.frame()
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      games <- jsonlite::fromJSON(jsonlite::toJSON(resp$dates), flatten = TRUE) %>% 
        tidyr::unnest(.data$games) %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::select(-.data$events)
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no umpire game data for {umpire_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(games)
}


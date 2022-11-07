#' @rdname mlb_game_context_metrics
#' @title **Acquire game context metrics for Major and Minor League games**
#'
#' @param game_pk The game_pk for the game requested
#' @param timecode The time code for the MLB game (format: MMDDYYYY_HHMMSS)

#' @importFrom jsonlite fromJSON
#' @return Returns a tibble that includes time codes from the game_pk requested
#' 
#'  |col_name                              |types     |
#'  |:-------------------------------------|:---------|
#'  |game_pk                               |integer   |
#'  |link                                  |character |
#'  |game_type                             |character |
#'  |season                                |character |
#'  |game_date                             |character |
#'  |official_date                         |character |
#'  |status_abstract_game_state            |character |
#'  |status_coded_game_state               |character |
#'  |status_detailed_state                 |character |
#'  |status_status_code                    |character |
#'  |status_start_time_tbd                 |logical   |
#'  |status_abstract_game_code             |character |
#'  |teams_away_league_record_wins         |integer   |
#'  |teams_away_league_record_losses       |integer   |
#'  |teams_away_league_record_pct          |character |
#'  |teams_away_score                      |integer   |
#'  |teams_away_team_id                    |integer   |
#'  |teams_away_team_name                  |character |
#'  |teams_away_team_link                  |character |
#'  |teams_away_is_winner                  |logical   |
#'  |teams_away_probable_pitcher_id        |integer   |
#'  |teams_away_probable_pitcher_full_name |character |
#'  |teams_away_probable_pitcher_link      |character |
#'  |teams_away_split_squad                |logical   |
#'  |teams_away_series_number              |integer   |
#'  |teams_home_league_record_wins         |integer   |
#'  |teams_home_league_record_losses       |integer   |
#'  |teams_home_league_record_pct          |character |
#'  |teams_home_score                      |integer   |
#'  |teams_home_team_id                    |integer   |
#'  |teams_home_team_name                  |character |
#'  |teams_home_team_link                  |character |
#'  |teams_home_is_winner                  |logical   |
#'  |teams_home_probable_pitcher_id        |integer   |
#'  |teams_home_probable_pitcher_full_name |character |
#'  |teams_home_probable_pitcher_link      |character |
#'  |teams_home_split_squad                |logical   |
#'  |teams_home_series_number              |integer   |
#'  |venue_id                              |integer   |
#'  |venue_name                            |character |
#'  |venue_link                            |character |
#'  |link_1                                |character |
#'  |is_tie                                |logical   |
#'  |game_number                           |integer   |
#'  |public_facing                         |logical   |
#'  |double_header                         |character |
#'  |gameday_type                          |character |
#'  |tiebreaker                            |character |
#'  |calendar_event_id                     |character |
#'  |season_display                        |character |
#'  |day_night                             |character |
#'  |scheduled_innings                     |integer   |
#'  |reverse_home_away_status              |logical   |
#'  |inning_break_length                   |integer   |
#'  |games_in_series                       |integer   |
#'  |series_game_number                    |integer   |
#'  |series_description                    |character |
#'  |record_source                         |character |
#'  |if_necessary                          |character |
#'  |if_necessary_description              |character |
#'  |game_id                               |character |
#'  |home_win_probability                  |numeric   |
#'  |away_win_probability                  |numeric   |
#'  
#' @export
#' @examples \donttest{
#'   try(mlb_game_context_metrics(game_pk = 531060, timecode = "20180803_182458"))
#' }

mlb_game_context_metrics <- function(
  game_pk, 
  timecode) {
  
  query_params <- list(  
    timecode = timecode
  )
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/game/{game_pk}/contextMetrics"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint,
                                   query = query_params)
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()  
      
      game <- resp$game  %>% 
        jsonlite::toJSON() %>% 
        jsonlite::fromJSON(flatten = TRUE) %>% 
        as.data.frame() 
      
      awayWinProbability <- data.frame(away_win_probability = resp$awayWinProbability)
      homeWinProbability <- data.frame(home_win_probability = resp$homeWinProbability)
      
      if(length(resp$rightFieldSacFlyProbability)>0){
        leftFieldSacFly <- resp$leftFieldSacFlyProbability %>% 
          as.data.frame() 
        
        centerFieldSacFly <- resp$centerFieldSacFlyProbability %>% 
          as.data.frame() 
        
        
        rightFieldSacFly <- resp$rightFieldSacFlyProbability %>% 
          as.data.frame() 
        context_metrics <- dplyr::bind_cols(game, 
                                            leftFieldSacFly, 
                                            centerFieldSacFly, 
                                            rightFieldSacFly,
                                            homeWinProbability,
                                            awayWinProbability)
      }else{
        context_metrics <- dplyr::bind_cols(game, 
                                            homeWinProbability,
                                            awayWinProbability)
      }
      context_metrics <- context_metrics %>% 
        janitor::clean_names() %>%
        make_baseballr_data("MLB Game Context Metrics data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(context_metrics)
}

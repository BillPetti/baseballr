#' @rdname mlb_game_context_metrics
#' @title **Acquire game context metrics for Major and Minor League games**
#'
#' @param game_pk The game_pk for the game requested
#' @param timecode The time code for the MLB game (format: MMDDYYYY_HHMMSS)

#' @importFrom jsonlite fromJSON
#' @return Returns a tibble that includes time codes from the game_pk requested
#' 
#'  |col_name                              |types     |description                                       |
#'  |:-------------------------------------|:---------|:-------------------------------------------------|
#'  |game_pk                               |integer   |Unique game identifier.                           |
#'  |game_guid                             |character |Globally unique game identifier.                  |
#'  |link                                  |character |MLB Stats API relative game link.                 |
#'  |game_type                             |character |Game type code (R, P, etc.).                      |
#'  |season                                |character |Season (YYYY).                                    |
#'  |game_date                             |character |Game date-time (ISO 8601, UTC).                   |
#'  |official_date                         |character |Official game date (YYYY-MM-DD).                  |
#'  |status_abstract_game_state            |character |Abstract game state (e.g. Final).                 |
#'  |status_coded_game_state               |character |Coded game state.                                 |
#'  |status_detailed_state                 |character |Detailed game state.                              |
#'  |status_status_code                    |character |Game status code.                                 |
#'  |status_start_time_tbd                 |logical   |Whether the start time is TBD.                    |
#'  |status_abstract_game_code             |character |Abstract game code.                               |
#'  |teams_away_team_id                    |integer   |Away team MLB ID.                                 |
#'  |teams_away_team_name                  |character |Away team name.                                   |
#'  |teams_away_team_link                  |character |MLB Stats API relative away team link.            |
#'  |teams_away_league_record_wins         |integer   |Away team league-record wins.                     |
#'  |teams_away_league_record_losses       |integer   |Away team league-record losses.                   |
#'  |teams_away_league_record_ties         |integer   |Away team league-record ties.                     |
#'  |teams_away_league_record_pct          |character |Away team winning percentage.                     |
#'  |teams_away_score                      |integer   |Away team score.                                  |
#'  |teams_away_is_winner                  |logical   |Whether the away team won.                        |
#'  |teams_away_probable_pitcher_id        |integer   |Away probable pitcher MLB ID.                     |
#'  |teams_away_probable_pitcher_full_name |character |Away probable pitcher name.                       |
#'  |teams_away_probable_pitcher_link      |character |MLB Stats API relative away pitcher link.         |
#'  |teams_away_split_squad                |logical   |Whether the away team is a split squad.           |
#'  |teams_away_series_number              |integer   |Away team series number.                          |
#'  |teams_home_team_id                    |integer   |Home team MLB ID.                                 |
#'  |teams_home_team_name                  |character |Home team name.                                   |
#'  |teams_home_team_link                  |character |MLB Stats API relative home team link.            |
#'  |teams_home_league_record_wins         |integer   |Home team league-record wins.                     |
#'  |teams_home_league_record_losses       |integer   |Home team league-record losses.                   |
#'  |teams_home_league_record_ties         |integer   |Home team league-record ties.                     |
#'  |teams_home_league_record_pct          |character |Home team winning percentage.                     |
#'  |teams_home_score                      |integer   |Home team score.                                  |
#'  |teams_home_is_winner                  |logical   |Whether the home team won.                        |
#'  |teams_home_probable_pitcher_id        |integer   |Home probable pitcher MLB ID.                     |
#'  |teams_home_probable_pitcher_full_name |character |Home probable pitcher name.                       |
#'  |teams_home_probable_pitcher_link      |character |MLB Stats API relative home pitcher link.         |
#'  |teams_home_split_squad                |logical   |Whether the home team is a split squad.           |
#'  |teams_home_series_number              |integer   |Home team series number.                          |
#'  |venue_id                              |integer   |Venue ID.                                         |
#'  |venue_name                            |character |Venue name.                                       |
#'  |venue_link                            |character |MLB Stats API relative venue link.                |
#'  |link_1                                |character |MLB Stats API relative game content link.         |
#'  |is_tie                                |logical   |Whether the game ended in a tie.                  |
#'  |game_number                           |integer   |Game number within a doubleheader.                |
#'  |public_facing                         |logical   |Whether the game is public-facing.                |
#'  |double_header                         |character |Doubleheader indicator (N/Y/S).                   |
#'  |gameday_type                          |character |Gameday data type code.                           |
#'  |tiebreaker                            |character |Tiebreaker indicator.                             |
#'  |calendar_event_id                     |character |Calendar event identifier.                        |
#'  |season_display                        |character |Display season (YYYY).                            |
#'  |day_night                             |character |Day/night designation.                            |
#'  |scheduled_innings                     |integer   |Number of scheduled innings.                      |
#'  |reverse_home_away_status              |logical   |Whether home/away designation is reversed.        |
#'  |inning_break_length                   |integer   |Length of the inning break (seconds).             |
#'  |games_in_series                       |integer   |Total games in the series.                        |
#'  |series_game_number                    |integer   |Game number within the series.                    |
#'  |series_description                    |character |Series description.                               |
#'  |record_source                         |character |Source of the record data.                        |
#'  |if_necessary                          |character |Whether the game is played only if necessary.     |
#'  |if_necessary_description              |character |If-necessary description.                         |
#'  |game_id                               |character |Human-readable game ID slug.                      |
#'  |home_win_probability                  |numeric   |Home team win probability (percent).              |
#'  |away_win_probability                  |numeric   |Away team win probability (percent).              |
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
  context_metrics <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()  
      
      game <- resp$game  |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE) |> 
        as.data.frame() 
      
      awayWinProbability <- data.frame(away_win_probability = resp$awayWinProbability)
      homeWinProbability <- data.frame(home_win_probability = resp$homeWinProbability)
      
      if(length(resp$rightFieldSacFlyProbability)>0){
        leftFieldSacFly <- resp$leftFieldSacFlyProbability |> 
          as.data.frame() 
        
        centerFieldSacFly <- resp$centerFieldSacFlyProbability |> 
          as.data.frame() 
        
        
        rightFieldSacFly <- resp$rightFieldSacFlyProbability |> 
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
      context_metrics <- context_metrics |> 
        janitor::clean_names() |>
        make_baseballr_data("MLB Game Context Metrics data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(context_metrics)
}

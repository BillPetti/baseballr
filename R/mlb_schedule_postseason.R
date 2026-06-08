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
#'
#' @importFrom jsonlite fromJSON
#' @return Returns a tibble that includes game_pk values and additional
#' information for games scheduled or played
#'
#'  |col_name                        |types     |description                                       |
#'  |:-------------------------------|:---------|:-------------------------------------------------|
#'  |date                            |character |Calendar date for the schedule entry.             |
#'  |total_items                     |integer   |Total schedule items on the date.                 |
#'  |total_events                    |integer   |Total non-game events on the date.                |
#'  |total_games                     |integer   |Total games on the date.                          |
#'  |total_games_in_progress         |integer   |Games currently in progress on the date.          |
#'  |game_pk                         |integer   |Unique game identifier.                           |
#'  |game_guid                       |character |Globally unique game identifier (GUID).           |
#'  |link                            |character |API link to the game feed.                        |
#'  |game_type                       |character |Postseason game type code (e.g. 'F', 'D', 'W').   |
#'  |season                          |character |Season the game belongs to.                       |
#'  |game_date                       |character |Game date-time in UTC (ISO 8601).                 |
#'  |official_date                   |character |Official game date (YYYY-MM-DD).                  |
#'  |is_tie                          |logical   |Whether the game ended in a tie.                  |
#'  |is_featured_game                |logical   |Whether the game is a featured game.              |
#'  |game_number                     |integer   |Game number within a doubleheader.                |
#'  |public_facing                   |logical   |Whether the game is public-facing.                |
#'  |double_header                   |character |Doubleheader indicator ('N', 'S', 'Y').          |
#'  |gameday_type                    |character |Gameday data feed type.                           |
#'  |tiebreaker                      |character |Whether the game is a tiebreaker.                 |
#'  |calendar_event_id               |character |Calendar event identifier.                        |
#'  |season_display                  |character |Display string for the season.                    |
#'  |day_night                       |character |Day or night game indicator.                      |
#'  |description                     |character |Series/round description (e.g. 'AL Wild Card Game').|
#'  |scheduled_innings               |integer   |Scheduled number of innings.                      |
#'  |reverse_home_away_status        |logical   |Whether home/away teams are reversed.             |
#'  |games_in_series                 |integer   |Number of games in the series.                    |
#'  |series_game_number              |integer   |Game number within the series.                    |
#'  |series_description              |character |Description of the series.                         |
#'  |record_source                   |character |Source of the schedule record.                    |
#'  |if_necessary                    |character |Whether the game is played only if necessary.     |
#'  |if_necessary_description        |character |Description of the if-necessary status.           |
#'  |status_abstract_game_state      |character |Abstract game state (e.g. 'Final').               |
#'  |status_coded_game_state         |character |Coded game state.                                 |
#'  |status_detailed_state           |character |Detailed game state.                              |
#'  |status_status_code              |character |Status code for the game.                         |
#'  |status_start_time_tbd           |logical   |Whether the start time is TBD.                    |
#'  |status_abstract_game_code       |character |Abstract game state code.                         |
#'  |teams_away_score                |integer   |Away team score.                                  |
#'  |teams_away_is_winner            |logical   |Whether the away team won.                        |
#'  |teams_away_split_squad          |logical   |Whether the away team is a split squad.           |
#'  |teams_away_series_number        |integer   |Away team's series number.                        |
#'  |teams_away_team_id              |integer   |Away team MLBAM ID.                               |
#'  |teams_away_team_name            |character |Away team name.                                   |
#'  |teams_away_team_link            |character |API link to the away team.                        |
#'  |teams_away_league_record_wins   |integer   |Away team series-record wins.                     |
#'  |teams_away_league_record_losses |integer   |Away team series-record losses.                   |
#'  |teams_away_league_record_pct    |character |Away team winning percentage.                     |
#'  |teams_home_score                |integer   |Home team score.                                  |
#'  |teams_home_is_winner            |logical   |Whether the home team won.                        |
#'  |teams_home_split_squad          |logical   |Whether the home team is a split squad.           |
#'  |teams_home_series_number        |integer   |Home team's series number.                        |
#'  |teams_home_team_id              |integer   |Home team MLBAM ID.                               |
#'  |teams_home_team_name            |character |Home team name.                                   |
#'  |teams_home_team_link            |character |API link to the home team.                        |
#'  |teams_home_league_record_wins   |integer   |Home team series-record wins.                     |
#'  |teams_home_league_record_losses |integer   |Home team series-record losses.                   |
#'  |teams_home_league_record_pct    |character |Home team winning percentage.                     |
#'  |venue_id                        |integer   |MLBAM venue ID.                                   |
#'  |venue_name                      |character |Venue name.                                       |
#'  |venue_link                      |character |API link to the venue.                            |
#'  |content_link                    |character |API link to the game content.                     |
#'  |inning_break_length             |integer   |Length of inning breaks in seconds.               |
#'  |reschedule_date                 |character |Reschedule date-time, if rescheduled.             |
#'  |reschedule_game_date            |character |Reschedule game date, if rescheduled.             |
#'  |status_reason                   |character |Reason for the game status (e.g. 'Rain').         |
#'  |rescheduled_from                |character |Original date-time the game was rescheduled from. |
#'  |rescheduled_from_date           |character |Original date the game was rescheduled from.      |
#'  |events                          |list      |Nested list of non-game events.                   |
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
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  
  games <- data.frame()
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      
      games <- jsonlite::fromJSON(jsonlite::toJSON(resp$dates),flatten = TRUE) |> 
        tidyr::unnest("games") |>
        as.data.frame() |>
        janitor::clean_names() |>
        make_baseballr_data("MLB Schedule - Post-season data from MLB.com",Sys.time())
      
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no MLB postseason schedule data available!")
    },
    finally = {
    }
  )
  
  return(games)
  
}

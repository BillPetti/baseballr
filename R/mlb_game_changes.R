#' @rdname mlb_game_changes
#' @title **Acquire time codes for Major and Minor League games**
#'
#' @param updated_since Updated since date time
#' @param sport_id Return division(s) for all divisions in a specific sport.
#' @importFrom jsonlite fromJSON
#' @return Returns a tibble that includes time codes from the game_pk requested
#' 
#'  |col_name                        |types     |description                                       |
#'  |:-------------------------------|:---------|:-------------------------------------------------|
#'  |date                            |character |Schedule date (YYYY-MM-DD).                       |
#'  |total_items                     |integer   |Total items for the date.                         |
#'  |total_events                    |integer   |Total events for the date.                        |
#'  |total_games                     |integer   |Total games for the date.                         |
#'  |total_games_in_progress         |integer   |Games currently in progress for the date.         |
#'  |game_pk                         |integer   |Unique game identifier.                           |
#'  |game_guid                       |character |Globally unique game identifier.                  |
#'  |link                            |character |MLB Stats API relative game link.                 |
#'  |game_type                       |character |Game type code (R, P, D, etc.).                   |
#'  |season                          |character |Season (YYYY).                                    |
#'  |game_date                       |character |Game date-time (ISO 8601, UTC).                   |
#'  |official_date                   |character |Official game date (YYYY-MM-DD).                  |
#'  |is_tie                          |logical   |Whether the game ended in a tie.                  |
#'  |game_number                     |integer   |Game number within a doubleheader.                |
#'  |public_facing                   |logical   |Whether the game is public-facing.                |
#'  |double_header                   |character |Doubleheader indicator (N/Y/S).                   |
#'  |gameday_type                    |character |Gameday data type code.                           |
#'  |tiebreaker                      |character |Tiebreaker indicator.                             |
#'  |calendar_event_id               |character |Calendar event identifier.                        |
#'  |season_display                  |character |Display season (YYYY).                            |
#'  |day_night                       |character |Day/night designation.                            |
#'  |description                     |character |Game description.                                 |
#'  |scheduled_innings               |integer   |Number of scheduled innings.                      |
#'  |reverse_home_away_status        |logical   |Whether home/away designation is reversed.        |
#'  |inning_break_length             |integer   |Length of the inning break (seconds).             |
#'  |games_in_series                 |integer   |Total games in the series.                        |
#'  |series_game_number              |integer   |Game number within the series.                    |
#'  |series_description              |character |Series description.                               |
#'  |record_source                   |character |Source of the record data.                        |
#'  |if_necessary                    |character |Whether the game is played only if necessary.     |
#'  |if_necessary_description        |character |If-necessary description.                         |
#'  |status_abstract_game_state      |character |Abstract game state (e.g. Final).                 |
#'  |status_coded_game_state         |character |Coded game state.                                 |
#'  |status_detailed_state           |character |Detailed game state.                              |
#'  |status_status_code              |character |Game status code.                                 |
#'  |status_start_time_tbd           |logical   |Whether the start time is TBD.                    |
#'  |status_abstract_game_code       |character |Abstract game code.                               |
#'  |teams_away_score                |integer   |Away team score.                                  |
#'  |teams_away_is_winner            |logical   |Whether the away team won.                        |
#'  |teams_away_split_squad          |logical   |Whether the away team is a split squad.           |
#'  |teams_away_series_number        |integer   |Away team series number.                          |
#'  |teams_away_team_id              |integer   |Away team MLB ID.                                 |
#'  |teams_away_team_name            |character |Away team name.                                   |
#'  |teams_away_team_link            |character |MLB Stats API relative away team link.            |
#'  |teams_away_league_record_wins   |integer   |Away team league-record wins.                     |
#'  |teams_away_league_record_losses |integer   |Away team league-record losses.                   |
#'  |teams_away_league_record_ties   |integer   |Away team league-record ties.                     |
#'  |teams_away_league_record_pct    |character |Away team winning percentage.                     |
#'  |teams_home_score                |integer   |Home team score.                                  |
#'  |teams_home_is_winner            |logical   |Whether the home team won.                        |
#'  |teams_home_split_squad          |logical   |Whether the home team is a split squad.           |
#'  |teams_home_series_number        |integer   |Home team series number.                          |
#'  |teams_home_team_id              |integer   |Home team MLB ID.                                 |
#'  |teams_home_team_name            |character |Home team name.                                   |
#'  |teams_home_team_link            |character |MLB Stats API relative home team link.            |
#'  |teams_home_league_record_wins   |integer   |Home team league-record wins.                     |
#'  |teams_home_league_record_losses |integer   |Home team league-record losses.                   |
#'  |teams_home_league_record_ties   |integer   |Home team league-record ties.                     |
#'  |teams_home_league_record_pct    |character |Home team winning percentage.                     |
#'  |venue_id                        |integer   |Venue ID.                                         |
#'  |venue_name                      |character |Venue name.                                       |
#'  |venue_link                      |character |MLB Stats API relative venue link.                |
#'  |content_link                    |character |MLB Stats API relative game content link.         |
#'  |status_reason                   |character |Reason for the game status, if any.               |
#'  |rescheduled_from                |character |Original scheduled date-time if rescheduled.      |
#'  |rescheduled_from_date           |character |Original scheduled date if rescheduled.           |
#'  |resumed_from                    |character |Original date-time if the game was resumed.       |
#'  |resumed_from_date               |character |Original date if the game was resumed.            |
#'  |events                          |list      |Nested list of change events for the game.        |
#'  
#' @export
#' @examples \donttest{
#'   try(mlb_game_changes(updated_since = "2021-08-10T19:08:24.000004Z", sport_id = 1))
#' }

mlb_game_changes <- function(updated_since, sport_id) {
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/game/changes"))
  
  query_params <- list( 
    sportId = sport_id,
    updatedSince = updated_since
  )
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  changes <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call() 
      
      changes <- resp$dates |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE) |> 
        tidyr::unnest("games") |>
        as.data.frame() |>
        janitor::clean_names() |>
        make_baseballr_data("MLB Game Changes data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(changes)
}

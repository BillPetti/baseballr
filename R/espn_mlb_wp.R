#' **Get MLB win probability chart data from ESPN**
#' @rdname espn_mlb_wp
#'
#' @param game_id (*Integer* required): Game ID filter for querying a single game
#'
#' @return [espn_mlb_wp()] - A `baseballr_data` tibble, one row per win-probability play:
#'
#'    |col_name |types |description |
#'    |:--------|:-----|:-----------|
#'    |game_id |numeric |Unique ESPN game/event identifier. |
#'    |play_id |character |Unique play identifier within the game. |
#'    |home_win_percentage |numeric |Home team win probability (0-1 decimal). |
#'    |away_win_percentage |numeric |Away team win probability (0-1 decimal). |
#'    |tie_percentage |numeric |Tie probability (0-1 decimal; extra-innings tie state). |
#'    |sequence_number |character |Sequence number of the play within the game. |
#'    |text |character |Text description of the play. |
#'    |away_score |integer |Away team run total after the play. |
#'    |home_score |integer |Home team run total after the play. |
#'    |scoring_play |logical |TRUE if the play scored a run. |
#'    |score_value |integer |Runs scored on the play. |
#'    |wallclock |character |Wall-clock timestamp of the play (ISO 8601). |
#'    |at_bat_id |character |Identifier of the at-bat the play belongs to. |
#'    |summary_type |character |Play summary type. |
#'    |outs |integer |Outs in the inning after the play. |
#'    |participants |list |List of athlete participants in the play. |
#'    |type_id |character |Play type identifier. |
#'    |type_text |character |Play type description. |
#'    |type_type |character |Play type category. |
#'    |period_type |character |Period type ('inning'). |
#'    |period_number |integer |Inning number. |
#'    |period_display_value |character |Inning display value (e.g. 'Top 1st'). |
#'    |team_id |character |Batting team identifier. |
#'    |pitch_count_balls |integer |Balls in the count when the pitch was thrown. |
#'    |pitch_count_strikes |integer |Strikes in the count when the pitch was thrown. |
#'    |result_count_balls |integer |Balls in the count after the pitch. |
#'    |result_count_strikes |integer |Strikes in the count after the pitch. |
#'    |bat_order |integer |Batting order spot of the batter. |
#'    |type_alternative_text |character |Alternative play type text. |
#'    |bats_type |character |Batter handedness type. |
#'    |bats_abbreviation |character |Batter handedness (L/R/S). |
#'    |bats_display_value |character |Batter handedness display value. |
#'    |at_bat_pitch_number |integer |Pitch number within the at-bat. |
#'    |pitch_velocity |integer |Pitch velocity (mph). |
#'    |trajectory |character |Batted-ball trajectory. |
#'    |type_abbreviation |character |Play type abbreviation. |
#'    |pitch_coordinate_x |integer |Pitch location x-coordinate. |
#'    |pitch_coordinate_y |integer |Pitch location y-coordinate. |
#'    |pitch_type_id |character |Pitch type identifier. |
#'    |pitch_type_text |character |Pitch type description. |
#'    |pitch_type_abbreviation |character |Pitch type abbreviation. |
#'    |hit_coordinate_x |integer |Batted-ball location x-coordinate. |
#'    |hit_coordinate_y |integer |Batted-ball location y-coordinate. |
#'    |alternative_play |character |Alternative play flag. |
#'    |alternative_type_id |character |Alternative play type id. |
#'    |alternative_type_text |character |Alternative play type text. |
#'    |alternative_type_abbreviation |character |Alternative play type abbreviation. |
#'    |alternative_type_alternative_text |character |Alternative play alternative text. |
#'    |alternative_type_type |character |Alternative play type category. |
#'    |on_first_athlete_id |character |Athlete id of the runner on first base. |
#'    |on_second_athlete_id |character |Athlete id of the runner on second base. |
#'    |on_third_athlete_id |character |Athlete id of the runner on third base. |
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_headers req_timeout req_retry req_perform resp_body_string
#' @importFrom utils URLencode URLdecode
#' @importFrom cli cli_abort
#' @importFrom janitor clean_names
#' @importFrom stringr str_sub str_length
#' @import dplyr
#' @export
#' @keywords MLB Win Probability Chart Data
#' @family ESPN MLB Functions
#' @examples
#' \donttest{
#' espn_mlb_wp(game_id = 401283399)
#' }
#'
espn_mlb_wp <- function(game_id) {
  .args <- mget(setdiff(names(formals()), "..."))
  if (!is.null(game_id) && !is.numeric(game_id)) {
    # Check if game_id is numeric, if not NULL
    cli::cli_abort("Enter valid game_id value (Integer)")
  }

  espn_game_id <- game_id

  espn_wp <- data.frame()

  tryCatch(
    expr = {
      espn_wp <-
        .retry_request(sprintf("http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?event=%s", espn_game_id)) %>%
        .resp_text() %>%
        jsonlite::fromJSON(flatten = TRUE)

      espn_wp_vals <- espn_wp %>%
        purrr::pluck("winprobability") %>%
        janitor::clean_names()
      espn_plays <- espn_wp$plays %>%
        jsonlite::toJSON() %>%
        jsonlite::fromJSON(flatten = TRUE) %>%
        janitor::clean_names() %>%
        dplyr::rename(
          "play_id" = "id"
        )
      espn_wp <- espn_wp_vals %>%
        dplyr::left_join(espn_plays, by = "play_id") %>%
        dplyr::mutate(
          away_win_percentage = 1 - .data$home_win_percentage - .data$tie_percentage,
          game_id = espn_game_id
        ) %>%
        dplyr::select(
          dplyr::any_of(c("game_id", "play_id", "home_win_percentage", "away_win_percentage", "tie_percentage")),
          tidyr::everything()
        ) %>%
        janitor::clean_names() %>%
        make_baseballr_data("ESPN MLB Win Probability Information from ESPN.com", Sys.time())
    },
    error = function(e) .report_api_error(
      e,
      hint = "game_id '{espn_game_id}' invalid or no ESPN win probability data available!",
      args = .args
    ),
    warning = function(w) {
    },
    finally = {
    }
  )
  return(espn_wp)
}

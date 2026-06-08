#' @rdname mlb_game_endpoints
#' @name mlb_game_endpoints
#' @aliases mlb_game_endpoints game pbp
#' @title
#' **MLB Game Endpoint Overview**
#' @description
#'
#' * `mlb_game_info()`: Retrieve additional game information for major and minor league games.
#' * `mlb_game_pks()`: Get MLB Game Info by Date and Level.
#' * `mlb_game_content()`: Retrieve additional game content for major and minor league games.
#' * `mlb_game_linescore()`: Retrieve game linescores for major and minor league games.
#' * `mlb_game_wp()`: Acquire win probability for Major and Minor League games.
#' * `mlb_game_pace()`: Retrieve game pace metrics for major and minor league.
#' * `mlb_game_changes()`: Acquire time codes for Major and Minor League games.
#' * `mlb_game_context_metrics()`: Acquire game context metrics for Major and Minor League games.
#' * `mlb_pbp()`: Acquire pitch-by-pitch data for Major and Minor League games.
#' * `mlb_pbp_diff()`: Acquire pitch-by-pitch data between two timecodes for Major and Minor League games.
#'
#' @details
#' ## **MLB Game**
#'
#' These functions retrieve MLB game information, content, linescores, win probability, pace, changes, context metrics, and pitch-by-pitch data from the MLB Stats API.
#'
#' @family MLB Game
NULL

#' @rdname mlb_game_info
#' @title **Retrieve additional game information for major and minor league games**
#' @param game_pk The unique game_pk identifier for the game
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @return Returns a tibble that includes supplemental information, such as
#' weather, official scorer, attendance, etc., for the game_pk provided
#'
#'  |col_name        |types     |description                                       |
#'  |:---------------|:---------|:-------------------------------------------------|
#'  |game_date       |character |Game date (YYYY-MM-DD).                           |
#'  |game_pk         |numeric   |Unique game identifier.                           |
#'  |venue_name      |character |Stadium name.                                     |
#'  |venue_id        |integer   |Venue ID.                                         |
#'  |temperature     |character |Game-time temperature (degrees F).                |
#'  |other_weather   |character |Weather condition description.                    |
#'  |wind            |character |Wind speed and direction.                         |
#'  |attendance      |character |Reported game attendance.                         |
#'  |start_time      |character |First-pitch local start time.                     |
#'  |elapsed_time    |character |Total elapsed game time (H:MM).                   |
#'  |game_id         |character |Human-readable game ID slug.                      |
#'  |game_type       |character |Game type code (R, P, etc.).                      |
#'  |home_sport_code |character |Home sport code (always 'mlb').                   |
#'  |official_scorer |character |Official scorer name.                             |
#'  |date            |character |Long-form game date label.                        |
#'  |status_ind      |character |Game status code.                                 |
#'  |home_league_id  |integer   |Home team league ID.                              |
#'  |gameday_sw      |character |Gameday data type code.                           |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_game_info(game_pk = 566001))
#' }

mlb_game_info <- function(game_pk) {
  
  api_call <- paste0("http://statsapi.mlb.com/api/v1.1/game/", game_pk,"/feed/live")
  
  game_table <- NULL
  tryCatch(
    expr = {
      payload <- jsonlite::fromJSON(api_call)
      
      lookup_table <- payload$liveData$boxscore$info |>
        as.data.frame() |>
        tidyr::spread(.data$label, .data$value)
      
      year <- stringr::str_sub(payload$gameData$game$calendarEventID, -10, -7)
      
      
      game_table <- tibble(game_date = stringr::str_sub(payload$gameData$game$calendarEventID,
                                                        -10, -1),
                           game_pk = game_pk,
                           venue_name = payload$gameData$venue$name,
                           venue_id = payload$gameData$venue$id,
                           temperature = payload$gameData$weather$temp,
                           other_weather = payload$gameData$weather$condition,
                           wind = payload$gameData$weather$wind,
                           attendance = ifelse("Att" %in% names(lookup_table) == TRUE,
                                               as.character(lookup_table$Att) |> 
                                                 stringr::str_remove_all('\\.'),
                                               NA),
                           start_time = as.character(lookup_table$`First pitch`) |>
                             stringr::str_remove_all('\\.'),
                           elapsed_time = as.character(lookup_table$T) |> 
                             stringr::str_remove_all('\\.'),
                           game_id = payload$gameData$game$id,
                           game_type = payload$gameData$game$type,
                           home_sport_code = "mlb",
                           official_scorer = payload$gameData$officialScorer$fullName,
                           date = names(lookup_table)[1],
                           status_ind = payload$gameData$status$statusCode,
                           home_league_id = payload$gameData$teams$home$league$id,
                           gameday_sw = payload$gameData$game$gamedayType) |>
        make_baseballr_data("MLB Game Info data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(game_table)
  
}

#' @rdname get_game_info_mlb 
#' @title **(legacy) Retrieve additional game information for major and minor league games**
#' @inheritParams mlb_game_info
#' @return Returns a tibble that includes supplemental information, such as
#' weather, official scorer, attendance, etc., for the game_pk provided
#' @keywords legacy
#' @export
get_game_info_mlb <- mlb_game_info

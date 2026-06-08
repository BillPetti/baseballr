#' @title **MLB Attendance** 
#' @param team_id Return attendance information for a particular team_id(s). 
#' @param league_id Return attendance information for a particular league_id(s). Format: '103,104'
#' @param season Return attendance information for particular year(s).
#' @param date Return attendance information on a particular date. Format: MM/DD/YYYY
#' @param league_list_id Unique league list identifier to return a directory of attendance for a specific league list_id
#'   Valid values include:
#'    * milb_full
#'    * milb_short
#'    * milb_complex
#'    * milb_all
#'    * milb_all_nomex
#'    * milb_all_domestic
#'    * milb_noncomp
#'    * milb_noncomp_nomex
#'    * milb_domcomp
#'    * milb_intcomp
#'    * win_noabl
#'    * win_caribbean
#'    * win_all
#'    * abl
#'    * mlb
#'    * mlb_hist
#'    * mlb_milb
#'    * mlb_milb_hist
#'    * mlb_milb_win
#'    * baseball_all
#' @return Returns a tibble with the following columns
#'
#'  |col_name                          |types     |description                                          |
#'  |:---------------------------------|:---------|:----------------------------------------------------|
#'  |openings_total                    |integer   |Total number of openings (games eligible to open).   |
#'  |openings_total_away               |integer   |Total away openings.                                 |
#'  |openings_total_home               |integer   |Total home openings.                                 |
#'  |openings_total_lost               |integer   |Openings lost (e.g. to rainouts).                    |
#'  |games_total                       |integer   |Total games played.                                  |
#'  |games_away_total                  |integer   |Total away games.                                    |
#'  |games_home_total                  |integer   |Total home games.                                    |
#'  |year                              |character |Season year (YYYY).                                  |
#'  |attendance_average_away           |integer   |Average away-game attendance.                        |
#'  |attendance_average_home           |integer   |Average home-game attendance.                        |
#'  |attendance_average_ytd            |integer   |Year-to-date average attendance.                     |
#'  |attendance_high                   |integer   |Highest single-game attendance.                      |
#'  |attendance_high_date              |character |Date of the highest-attendance game.                 |
#'  |attendance_low                    |integer   |Lowest single-game attendance.                       |
#'  |attendance_low_date               |character |Date of the lowest-attendance game.                  |
#'  |attendance_opening_average        |integer   |Average opening-day attendance.                      |
#'  |attendance_total                  |integer   |Total attendance for the period.                     |
#'  |attendance_total_away             |integer   |Total away attendance.                               |
#'  |attendance_total_home             |integer   |Total home attendance.                               |
#'  |attendance_high_game_game_pk      |integer   |game_pk of the highest-attendance game.              |
#'  |attendance_high_game_link         |character |API link to the highest-attendance game.             |
#'  |attendance_high_game_day_night    |character |Day/night status of the highest-attendance game.     |
#'  |attendance_high_game_content_link |character |API content link for the highest-attendance game.    |
#'  |attendance_low_game_game_pk       |integer   |game_pk of the lowest-attendance game.               |
#'  |attendance_low_game_link          |character |API link to the lowest-attendance game.              |
#'  |attendance_low_game_day_night     |character |Day/night status of the lowest-attendance game.      |
#'  |attendance_low_game_content_link  |character |API content link for the lowest-attendance game.     |
#'  |game_type_id                      |character |Game type code (e.g. R for regular season).          |
#'  |game_type_description             |character |Game type description.                               |
#'  |team_id                           |integer   |MLB team ID.                                         |
#'  |team_name                         |character |Team name.                                           |
#'  |team_link                         |character |MLB Stats API relative team link.                    |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_attendance(team_id = 109, season = 2021))
#' }
mlb_attendance <- function(
  team_id = NULL, 
  league_id = NULL,  
  season = NULL,  
  date = NULL,  
  league_list_id = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/attendance")
  query_params <- list(
    teamId = team_id, 
    leagueId = league_id,  
    season = season,  
    date = date,  
    leagueListId = league_list_id
  )
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  records <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      records <- jsonlite::fromJSON(jsonlite::toJSON(resp$records), flatten = TRUE)  |> 
        janitor::clean_names() |>
        make_baseballr_data("MLB Attendance data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  return(records)
}

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
#' @return Returns a data frame with the following columns
#'  |col_name                          |types     |
#'  |:---------------------------------|:---------|
#'  |openings_total                    |integer   |
#'  |openings_total_away               |integer   |
#'  |openings_total_home               |integer   |
#'  |openings_total_lost               |integer   |
#'  |games_total                       |integer   |
#'  |games_away_total                  |integer   |
#'  |games_home_total                  |integer   |
#'  |year                              |character |
#'  |attendance_average_away           |integer   |
#'  |attendance_average_home           |integer   |
#'  |attendance_average_ytd            |integer   |
#'  |attendance_high                   |integer   |
#'  |attendance_high_date              |character |
#'  |attendance_low                    |integer   |
#'  |attendance_low_date               |character |
#'  |attendance_opening_average        |integer   |
#'  |attendance_total                  |integer   |
#'  |attendance_total_away             |integer   |
#'  |attendance_total_home             |integer   |
#'  |attendance_high_game_game_pk      |integer   |
#'  |attendance_high_game_link         |character |
#'  |attendance_high_game_day_night    |character |
#'  |attendance_high_game_content_link |character |
#'  |attendance_low_game_game_pk       |integer   |
#'  |attendance_low_game_link          |character |
#'  |attendance_low_game_day_night     |character |
#'  |attendance_low_game_content_link  |character |
#'  |game_type_id                      |character |
#'  |game_type_description             |character |
#'  |team_id                           |integer   |
#'  |team_name                         |character |
#'  |team_link                         |character |
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
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      records <- jsonlite::fromJSON(jsonlite::toJSON(resp$records), flatten = TRUE)  %>% 
        janitor::clean_names() %>%
        make_baseballr_data("MLB Attendance data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(records)
}
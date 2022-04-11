#' @rdname mlb_game_wp
#' @title **Acquire win probability for Major and Minor League games**
#'
#' @param game_pk The game_pk for the game requested
#' @param timecode The time code for the MLB game (format: MMDDYYYY_HHMMSS)

#' @importFrom jsonlite fromJSON
#' @return Returns a data frame that includes time codes from the game_pk requested
#' 
#'  |col_name                        |types   |
#'  |:-------------------------------|:-------|
#'  |home_team_win_probability       |numeric |
#'  |away_team_win_probability       |numeric |
#'  |home_team_win_probability_added |numeric |
#'  |at_bat_index                    |integer |
#'  |leverage_index                  |numeric |
#'  
#' @export
#' @examples \donttest{
#'   try(mlb_game_wp(game_pk = 531060))
#' }

mlb_game_wp <- function(game_pk, 
                        timecode = NULL) {
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/game/{game_pk}/winProbability"))
  query_params <- list(  
    timecode = timecode,
    fields = "atBatIndex,homeTeamWinProbability,awayTeamWinProbability,homeTeamWinProbabilityAdded,leverageIndex"
  )
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call() 
      
      wp <- resp %>% 
        jsonlite::toJSON() %>% 
        jsonlite::fromJSON(flatten = TRUE) %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>%
        make_baseballr_data("MLB Game Win Probability data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(wp)
}

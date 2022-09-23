#' @title **Find MLB Rosters by Roster Type**
#' @param team_id team_id to return team roster information for a particular club.
#' @param season Year to return team roster information for a particular club in a specific season.
#' @param date Date to return team roster and their coaching staff directorial information for a particular team. 
#' @param roster_type roster_type to return team directorial information for. See ```mlb_roster_types()``` for more options.
#'   Valid options include: '40Man', 'fullSeason', 'fullRoster', 'nonRosterInvitees', 'active', 
#'     'allTime', 'depthChart', 'gameday', 'coach'
#' @return Returns a tibble with the following columns:
#'  |col_name              |types     |
#'  |:---------------------|:---------|
#'  |jersey_number         |character |
#'  |person_id             |integer   |
#'  |person_full_name      |character |
#'  |person_link           |character |
#'  |position_code         |character |
#'  |position_name         |character |
#'  |position_type         |character |
#'  |position_abbreviation |character |
#'  |status_code           |character |
#'  |status_description    |character |
#'  |link                  |character |
#'  |team_id               |integer   |
#'  |roster_type           |character |
#'  |season                |numeric   |
#'  |date                  |character |
#'  
#' @export
#' @examples \donttest{
#'   try(mlb_rosters(team_id = 109, season = 2018, roster_type = 'active'))
#'   try(mlb_rosters(team_id = 109, season = 2018, roster_type = 'coach'))
#' }
mlb_rosters <- function(team_id = NULL, season = NULL, date = NULL, roster_type = NULL){

  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}/roster/{roster_type}"))
  query_params <- list(
    season = season,
    date = date
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      roster <- jsonlite::fromJSON(jsonlite::toJSON(resp$roster), flatten=TRUE) %>% 
        dplyr::bind_cols(link = resp$link, 
                         team_id = resp$teamId,
                         roster_type = resp$rosterType,
                         season = season,
                         date = ifelse(is.null(date),NA_character_,date)) %>% 
        janitor::clean_names() %>%
        make_baseballr_data("MLB Roster data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no roster data for {team_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(roster)
}

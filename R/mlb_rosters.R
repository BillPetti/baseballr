#' @title **Find MLB Rosters by Roster Type**
#' @param team_id team_id to return team roster information for a particular club.
#' @param season Year to return team roster information for a particular club in a specific season.
#' @param date Date to return team roster and their coaching staff directorial information for a particular team. 
#' @param roster_type roster_type to return team directorial information for. See ```mlb_roster_types()``` for more options.
#'   Valid options include: '40Man', 'fullSeason', 'fullRoster', 'nonRosterInvitees', 'active', 
#'     'allTime', 'depthChart', 'gameday', 'coach'
#' @return Returns a tibble with one row per roster member with the following
#'   columns (player roster types). The `coach` roster type instead returns
#'   `job`, `job_id`, and `title` columns in place of the position/status
#'   columns:
#'
#'  |col_name              |types     |description                                       |
#'  |:---------------------|:---------|:-------------------------------------------------|
#'  |jersey_number         |character |Player's uniform number.                          |
#'  |person_id             |integer   |MLBAM player ID.                                  |
#'  |person_full_name      |character |Player's full name.                               |
#'  |person_link           |character |API link to the player resource.                  |
#'  |position_code         |character |Numeric scorekeeping position code.               |
#'  |position_name         |character |Full position name.                               |
#'  |position_type         |character |Position category (e.g. 'Infielder').             |
#'  |position_abbreviation |character |Position abbreviation (e.g. 'SS').                |
#'  |status_code           |character |Roster status code (e.g. 'A').                    |
#'  |status_description    |character |Roster status description (e.g. 'Active').        |
#'  |link                  |character |API link to the team roster resource.             |
#'  |team_id               |integer   |MLBAM team ID.                                    |
#'  |roster_type           |character |Roster type returned.                             |
#'  |season                |numeric   |Season requested.                                 |
#'  |date                  |character |Date requested, or NA if not supplied.            |
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
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  roster <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      roster <- jsonlite::fromJSON(jsonlite::toJSON(resp$roster), flatten = TRUE) |> 
        dplyr::bind_cols(link = resp$link, 
                         team_id = resp$teamId,
                         roster_type = resp$rosterType,
                         season = season,
                         date = ifelse(is.null(date),NA_character_,date)) |> 
        janitor::clean_names() |>
        make_baseballr_data("MLB Roster data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no roster data for {team_id} available!")
    },
    finally = {
    }
  )
  return(roster)
}

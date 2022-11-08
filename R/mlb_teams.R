#' @title **MLB Teams** 
#' @param season Year to return to return team information for.
#' @param active_status The active statuses to populate teams for a given season.
#' @param all_star_statuses The all-star statuses to populate teams for a given season.
#' @param league_ids The league_id(s) to return team information for.
#' @param sport_ids The sport_id(s) to return team information for.
#' @param game_type The game_type to return team information for.
#' @return Returns a tibble with the following columns
#'  |col_name                   |types     |
#'  |:--------------------------|:---------|
#'  |team_id                    |integer   |
#'  |team_full_name             |character |
#'  |link                       |character |
#'  |season                     |integer   |
#'  |team_code                  |character |
#'  |file_code                  |character |
#'  |team_abbreviation          |character |
#'  |team_name                  |character |
#'  |location_name              |character |
#'  |first_year_of_play         |character |
#'  |short_name                 |character |
#'  |franchise_name             |character |
#'  |club_name                  |character |
#'  |all_star_status            |character |
#'  |active                     |logical   |
#'  |venue_id                   |integer   |
#'  |venue_name                 |character |
#'  |venue_link                 |character |
#'  |spring_venue_id            |integer   |
#'  |spring_venue_link          |character |
#'  |league_id                  |integer   |
#'  |league_name                |character |
#'  |league_link                |character |
#'  |division_id                |integer   |
#'  |division_name              |character |
#'  |division_link              |character |
#'  |sport_id                   |integer   |
#'  |sport_link                 |character |
#'  |sport_name                 |character |
#'  |spring_league_id           |integer   |
#'  |spring_league_name         |character |
#'  |spring_league_link         |character |
#'  |spring_league_abbreviation |character |
#' @export
#' @examples \donttest{
#'   try(mlb_teams(season = 2021, sport_ids = c(1)))
#' }
mlb_teams <- function(
  season = NULL, 
  active_status = NULL, 
  all_star_statuses = NULL,
  league_ids = NULL,
  sport_ids = NULL,
  game_type = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/teams")
  query_params <- list(
    season = season, 
    activeStatus = active_status, 
    allStarStatuses = all_star_statuses,
    leagueIds = league_ids,
    sportIds = sport_ids,
    gameType = game_type
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      teams <- jsonlite::fromJSON(jsonlite::toJSON(resp$teams),flatten = TRUE) %>% 
        janitor::clean_names() %>% 
        dplyr::rename(
          "team_id" = "id",
          "team_full_name" = "name",
          "team_abbreviation" = "abbreviation") %>%
        make_baseballr_data("MLB Teams data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(teams)
}

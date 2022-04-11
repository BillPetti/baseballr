#' @title **MLB Team Info** 
#' @param team_id The team_id to return team data for.
#' @param season The season to return team data for the given year.
#' @param sport_id The sport_id to return a directory of team data for a particular club in a sport.
#' @return Returns a data frame with the following columns
#'   |col_name           |types     |
#'   |:------------------|:---------|
#'   |all_star_status    |character |
#'   |team_id            |integer   |
#'   |team_full_name     |character |
#'   |link               |character |
#'   |season             |integer   |
#'   |team_code          |character |
#'   |file_code          |character |
#'   |team_abbreviation  |character |
#'   |team_name          |character |
#'   |location_name      |character |
#'   |first_year_of_play |character |
#'   |short_name         |character |
#'   |franchise_name     |character |
#'   |club_name          |character |
#'   |active             |logical   |
#'   |venue_id           |integer   |
#'   |venue_name         |character |
#'   |venue_link         |character |
#'   |spring_venue_id    |integer   |
#'   |spring_venue_link  |character |
#'   |league_id          |integer   |
#'   |league_name        |character |
#'   |league_link        |character |
#'   |sport_id           |integer   |
#'   |sport_link         |character |
#'   |sport_name         |character |
#' @export
#' @examples \donttest{
#'   try(mlb_team_info(team_id = 147))
#' }
mlb_team_info <- function(team_id = NULL, 
                          season = NULL, 
                          sport_id = NULL){
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}"))
  query_params <- list(
    season = season, 
    sportId = sport_id
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      teams <- jsonlite::fromJSON(jsonlite::toJSON(resp$teams),flatten = TRUE) %>% 
        janitor::clean_names() %>% 
        dplyr::rename(
          team_id = .data$id,
          team_full_name = .data$name,
          team_abbreviation = .data$abbreviation) %>%
        make_baseballr_data("MLB Team Info data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(teams)
}
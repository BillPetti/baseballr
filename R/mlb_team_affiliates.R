#' @title **MLB Team Affiliates** 
#' @param team_ids The team_id(s) to return affiliates data for.
#' @param season The season to return team affiliates data for the particular season.
#' @param sport_ids The sport_id to return team affiliates information for.
#' @return Returns a tibble with the following columns
#'    |col_name                   |types     |
#'    |:--------------------------|:---------|
#'    |all_star_status            |character |
#'    |team_id                    |integer   |
#'    |team_full_name             |character |
#'    |link                       |character |
#'    |season                     |integer   |
#'    |team_code                  |character |
#'    |file_code                  |character |
#'    |team_abbreviation          |character |
#'    |team_name                  |character |
#'    |location_name              |character |
#'    |first_year_of_play         |character |
#'    |short_name                 |character |
#'    |franchise_name             |character |
#'    |club_name                  |character |
#'    |active                     |logical   |
#'    |parent_org_name            |character |
#'    |parent_org_id              |integer   |
#'    |spring_league_id           |integer   |
#'    |spring_league_name         |character |
#'    |spring_league_link         |character |
#'    |spring_league_abbreviation |character |
#'    |venue_id                   |integer   |
#'    |venue_name                 |character |
#'    |venue_link                 |character |
#'    |spring_venue_id            |integer   |
#'    |spring_venue_link          |character |
#'    |league_id                  |integer   |
#'    |league_name                |character |
#'    |league_link                |character |
#'    |division_id                |integer   |
#'    |division_name              |character |
#'    |division_link              |character |
#'    |sport_id                   |integer   |
#'    |sport_link                 |character |
#'    |sport_name                 |character |
#' @export
#' @examples \donttest{
#'   try(mlb_team_affiliates(team_ids = 147))
#' }
mlb_team_affiliates <- function(
  team_ids = NULL,
  sport_ids = NULL, 
  season = NULL){
  team_ids <- paste(team_ids, collapse = ',')
  sport_ids <- paste(sport_ids, collapse = ',')
  mlb_endpoint <- mlb_stats_endpoint("v1/teams/affiliates")
  query_params <- list(
    teamIds = team_ids, 
    sportIds = sport_ids, 
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      teams <- jsonlite::fromJSON(jsonlite::toJSON(resp$teams),flatten = TRUE) %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          team_id = .data$id,
          team_full_name = .data$name,
          team_abbreviation = .data$abbreviation) %>%
        make_baseballr_data("MLB Team Affiliates data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(teams)
}

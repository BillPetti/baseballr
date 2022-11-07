#' @title **MLB Teams History** 
#' @param team_ids The team_id(s) to return historical data for.
#' @param start_season The start_season to return historical data for from the given year to present.
#' @param end_season The end_season to return historical data for from the the creation to the given year.
#' @return Returns a tibble with the following columns
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
#'   try(mlb_team_history(team_ids = 147))
#' }
mlb_team_history <- function(team_ids = NULL, 
                             start_season = NULL, 
                             end_season = NULL){
  team_ids <- paste(team_ids, collapse = ',')
  mlb_endpoint <- mlb_stats_endpoint("v1/teams/history")
  query_params <- list(
    teamIds = team_ids, 
    startSeason = start_season, 
    endSeason = end_season
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
        make_baseballr_data("MLB Team History data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(teams)
}

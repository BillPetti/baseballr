#' @title **Find MLB Seasons all**
#'
#' @param sport_id The sport_id to return season information for.
#' @param division_id The division_id to return season information for.
#' @param league_id The league_id to return season information for.
#' @param with_game_type_dates with_game_type_dates to return season information for.
#' @importFrom jsonlite fromJSON
#' @return Returns a tibble with the following columns:
#' 
#'  |col_name                    |types     |
#'  |:---------------------------|:---------|
#'  |season_id                   |character |
#'  |has_wildcard                |logical   |
#'  |pre_season_start_date       |character |
#'  |season_start_date           |character |
#'  |regular_season_start_date   |character |
#'  |regular_season_end_date     |character |
#'  |season_end_date             |character |
#'  |offseason_start_date        |character |
#'  |off_season_end_date         |character |
#'  |season_level_gameday_type   |character |
#'  |game_level_gameday_type     |character |
#'  |qualifier_plate_appearances |numeric   |
#'  |qualifier_outs_pitched      |integer   |
#'  |post_season_start_date      |character |
#'  |post_season_end_date        |character |
#'  |last_date1st_half           |character |
#'  |all_star_date               |character |
#'  |first_date2nd_half          |character |
#'  |pre_season_end_date         |character |
#'  |spring_start_date           |character |
#'  |spring_end_date             |character |
#'  
#' @export
#'
#' @examples \donttest{
#'  mlb_seasons_all(sport_id = 1)
#' }

mlb_seasons_all <- function(sport_id = 1,
                            division_id = NULL,
                            league_id = NULL,
                            with_game_type_dates = TRUE){
  mlb_endpoint <- mlb_stats_endpoint("v1/seasons/all")
  query_params <- list(
    withGameTypeDates = ifelse(with_game_type_dates==TRUE, 'true','false'),
    sportId = sport_id,
    divisionId = division_id,
    leagueId = league_id
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      
      seasons <- jsonlite::fromJSON(jsonlite::toJSON(resp$seasons),flatten = TRUE) %>% 
        as.data.frame() %>%
        janitor::clean_names() %>%
        make_baseballr_data("MLB Seasons - All Seasons data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(seasons)
  
}

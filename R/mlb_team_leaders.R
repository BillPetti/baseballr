#' @title **MLB Team Leaders**
#' @param team_id Team ID to return team leader information for.
#' @param leader_categories Team leader category to return information and ranking for a particular statistic.
#' @param leader_game_types Game type to return information and ranking for a particular statistic in a particular game type.
#' @param season Season to return team leader information for.
#' @param limit A limit to limit return to a particular number of records.
#' @return Returns a tibble with the following columns
#'   |col_name              |types     |
#'   |:---------------------|:---------|
#'   |leader_category       |character |
#'   |rank                  |integer   |
#'   |value                 |character |
#'   |season                |character |
#'   |team_id               |integer   |
#'   |team_name             |character |
#'   |team_link             |character |
#'   |league_id             |integer   |
#'   |league_name           |character |
#'   |league_link           |character |
#'   |person_id             |integer   |
#'   |person_full_name      |character |
#'   |person_link           |character |
#'   |person_first_name     |character |
#'   |person_last_name      |character |
#'   |sport_id              |integer   |
#'   |sport_link            |character |
#'   |sport_abbreviation    |character |
#'   |stat_group            |character |
#'   |total_splits          |integer   |
#'   |game_type_id          |character |
#'   |game_type_description |character |
#' @export
#' @examples \donttest{
#'   try(mlb_team_leaders(team_id = 137, leader_categories = "homeRuns", season = 2021))
#' }
mlb_team_leaders <- function(team_id = NULL,
                             leader_categories = NULL,
                             leader_game_types = NULL,
                             season = NULL,
                             limit = 1000){
  
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}/leaders"))
  query_params <- list(
    leaderCategories = leader_categories,
    leaderGameTypes = leader_game_types,
    season = season,
    limit = limit
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      team_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['teamLeaders']]), flatten = TRUE)  
      team_leaders$season <- NULL
      team_leaders$team.id <- NULL
      team_leaders$team.name <- NULL
      team_leaders$team.link <- NULL
      team_leaders <- team_leaders %>% 
        tidyr::unnest(.data$leaders) %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>%
        make_baseballr_data("MLB Team Leaders data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  
  return(team_leaders)
}


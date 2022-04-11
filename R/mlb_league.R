#' @title **MLB Leagues** 
#' @param seasons Year(s) to return to return league information for.
#' @param sport_id The sport_id to return league information for.
#' @param league_id The league_id(s) to return league information for.
#' @return Returns a tibble with the following columns
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |leader_type   |character |
#' @export
#' @examples \donttest{
#'   try(mlb_league(seasons = 2021, sport_id = 1))
#' }
mlb_league <- function(seasons = NULL,
                       sport_id = NULL, 
                       league_id = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/league")
  query_params <- list(
    seasons = seasons, 
    leagueIds = league_id,
    sportId = sport_id
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      
      leagues <- jsonlite::fromJSON(jsonlite::toJSON(resp), flatten = TRUE)$leagues %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::rename(
          league_id = .data$id,
          league_name = .data$name,
          league_link = .data$link,
          league_abbreviation = .data$abbreviation,
          league_name_short = .data$name_short,
          league_season_state = .data$season_state,
          league_has_wild_card = .data$has_wild_card,
          league_has_split_season = .data$has_split_season,
          league_num_games = .data$num_games,
          league_has_playoff_points = .data$has_playoff_points,
          league_num_teams = .data$num_teams,
          league_num_wildcard_teams = .data$num_wildcard_teams,
          league_season = .data$season,
          league_org_code = .data$org_code,
          league_conferences_in_use = .data$conferences_in_use,
          league_divisions_in_use = .data$divisions_in_use,
          league_sort_order = .data$sort_order,
          league_active = .data$active) %>%
        make_baseballr_data("MLB League data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(leagues)
}


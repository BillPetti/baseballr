#' @title **MLB Standings** 
#' @param season Year(s) to return to return standings information for.
#' @param date Date to return to return standings information for.
#' @param standings_type The standings_type(s) to return standings information for.
#'  **Description of all standings_types**
#'   1. regularSeason - Regular Season Standings
#'   2. wildCard - Wild card standings
#'   3. divisionLeaders - Division Leader standings
#'   4. wildCardWithLeaders - Wild card standings with Division Leaders
#'   5. firstHalf - First half standings.  Only valid for leagues with a split season (Mexican League).
#'   6. secondHalf - Second half standings. Only valid for leagues with a split season (Mexican League).
#'   7. springTraining - Spring Training Standings
#'   8. postseason - Postseason Standings
#'   9. byDivision - Standings by Division
#'   10. byConference - Standings by Conference
#'   11. byLeague - Standings by League
#' @param league_id The league_id(s) to return standings information for.
#' @return Returns a tibble with the following columns
#'  |col_name                                  |types     |
#'  |:-----------------------------------------|:---------|
#'  |standings_type                            |character |
#'  |last_updated                              |character |
#'  |team_records_season                       |character |
#'  |team_records_clinch_indicator             |character |
#'  |team_records_division_rank                |character |
#'  |team_records_league_rank                  |character |
#'  |team_records_sport_rank                   |character |
#'  |team_records_games_played                 |integer   |
#'  |team_records_games_back                   |character |
#'  |team_records_wild_card_games_back         |character |
#'  |team_records_league_games_back            |character |
#'  |team_records_spring_league_games_back     |character |
#'  |team_records_sport_games_back             |character |
#'  |team_records_division_games_back          |character |
#'  |team_records_conference_games_back        |character |
#'  |team_records_last_updated                 |character |
#'  |team_records_runs_allowed                 |integer   |
#'  |team_records_runs_scored                  |integer   |
#'  |team_records_division_champ               |logical   |
#'  |team_records_division_leader              |logical   |
#'  |team_records_has_wildcard                 |logical   |
#'  |team_records_clinched                     |logical   |
#'  |team_records_elimination_number           |character |
#'  |team_records_wild_card_elimination_number |character |
#'  |team_records_magic_number                 |character |
#'  |team_records_wins                         |integer   |
#'  |team_records_losses                       |integer   |
#'  |team_records_run_differential             |integer   |
#'  |team_records_winning_percentage           |character |
#'  |team_records_wild_card_rank               |character |
#'  |team_records_wild_card_leader             |logical   |
#'  |team_records_team_id                      |integer   |
#'  |team_records_team_name                    |character |
#'  |team_records_team_link                    |character |
#'  |team_records_streak_streak_type           |character |
#'  |team_records_streak_streak_number         |integer   |
#'  |team_records_streak_streak_code           |character |
#'  |team_records_league_record_wins           |integer   |
#'  |team_records_league_record_losses         |integer   |
#'  |team_records_league_record_ties           |integer   |
#'  |team_records_league_record_pct            |character |
#'  |team_records_records_split_records        |list      |
#'  |team_records_records_division_records     |list      |
#'  |team_records_records_overall_records      |list      |
#'  |team_records_records_league_records       |list      |
#'  |team_records_records_expected_records     |list      |
#'  |league_id                                 |integer   |
#'  |league_link                               |character |
#'  |division_id                               |integer   |
#'  |division_link                             |character |
#'  |sport_id                                  |integer   |
#'  |sport_link                                |character |
#'  
#' @export
#' @examples \donttest{
#'   try(mlb_standings(season = 2021, league_id = 103))
#' }
mlb_standings <- function(
  season = NULL,
  date = NULL,
  standings_type = NULL, 
  league_id = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/standings")
  query_params <- list(
    season = season, 
    date = date,
    leagueId = league_id,
    standingsTypes = standings_type
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      
      standings <- jsonlite::fromJSON(jsonlite::toJSON(resp$records), flatten = TRUE) %>% 
        tidyr::unnest("teamRecords", names_sep = "_") %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>%
        make_baseballr_data("MLB Standings data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  
  # split_standings <- purrr::map_df(1:length(standings$teamRecords_records.splitRecords),
  #                                  function(x){
  #                                    standings$teamRecords_records.splitRecords[[x]] %>% 
  #                                      tidyr::pivot_wider(names_from='type',names_glue = "{type}_{.value}",values_from = c("wins","losses", "pct"))
  #                                  })
  return(standings)
}


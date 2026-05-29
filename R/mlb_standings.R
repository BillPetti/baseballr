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
#'  |col_name                                   |types     |description                                              |
#'  |:------------------------------------------|:---------|:--------------------------------------------------------|
#'  |standings_type                             |character |Type of standings returned (e.g., regularSeason).        |
#'  |last_updated                               |character |Timestamp the standings record was last updated.         |
#'  |team_records_season                        |character |Season for the team record.                              |
#'  |team_records_clinch_indicator              |character |Clinch status indicator code.                            |
#'  |team_records_division_rank                 |character |Rank within the division.                                |
#'  |team_records_league_rank                   |character |Rank within the league.                                  |
#'  |team_records_sport_rank                    |character |Rank across the sport.                                   |
#'  |team_records_games_played                  |integer   |Games played.                                            |
#'  |team_records_games_back                    |character |Games back of the division leader.                       |
#'  |team_records_wild_card_games_back          |character |Games back of the wild card.                             |
#'  |team_records_league_games_back             |character |Games back within the league.                            |
#'  |team_records_spring_league_games_back      |character |Games back within the spring league.                     |
#'  |team_records_sport_games_back              |character |Games back across the sport.                             |
#'  |team_records_division_games_back           |character |Games back within the division.                          |
#'  |team_records_conference_games_back         |character |Games back within the conference.                        |
#'  |team_records_last_updated                  |character |Timestamp the team record was last updated.              |
#'  |team_records_runs_allowed                  |integer   |Total runs allowed.                                      |
#'  |team_records_runs_scored                   |integer   |Total runs scored.                                       |
#'  |team_records_division_champ                |logical   |Whether the team clinched the division.                  |
#'  |team_records_division_leader               |logical   |Whether the team leads its division.                     |
#'  |team_records_has_wildcard                  |logical   |Whether the team holds a wild card spot.                 |
#'  |team_records_clinched                      |logical   |Whether the team has clinched a playoff berth.           |
#'  |team_records_elimination_number            |character |Overall elimination number.                              |
#'  |team_records_elimination_number_sport      |character |Elimination number across the sport.                     |
#'  |team_records_elimination_number_league     |character |Elimination number within the league.                    |
#'  |team_records_elimination_number_division   |character |Elimination number within the division.                  |
#'  |team_records_elimination_number_conference |character |Elimination number within the conference.                |
#'  |team_records_wild_card_elimination_number  |character |Wild card elimination number.                            |
#'  |team_records_magic_number                  |character |Magic number to clinch.                                  |
#'  |team_records_wins                          |integer   |Total wins.                                              |
#'  |team_records_losses                        |integer   |Total losses.                                            |
#'  |team_records_run_differential              |integer   |Run differential (runs scored minus allowed).            |
#'  |team_records_winning_percentage            |character |Winning percentage.                                      |
#'  |team_records_wild_card_rank                |character |Rank in the wild card race.                              |
#'  |team_records_wild_card_leader              |logical   |Whether the team leads the wild card race.               |
#'  |team_records_team_id                       |integer   |Team MLBAM ID.                                           |
#'  |team_records_team_name                     |character |Team name.                                               |
#'  |team_records_team_link                     |character |API link to the team.                                    |
#'  |team_records_streak_streak_code            |character |Current streak code (e.g., W3, L1).                      |
#'  |team_records_streak_streak_type            |character |Streak type (wins or losses).                            |
#'  |team_records_streak_streak_number          |integer   |Length of the current streak.                            |
#'  |team_records_league_record_wins            |integer   |Wins in league play.                                     |
#'  |team_records_league_record_losses          |integer   |Losses in league play.                                   |
#'  |team_records_league_record_ties            |integer   |Ties in league play.                                     |
#'  |team_records_league_record_pct             |character |League play winning percentage.                          |
#'  |team_records_records_split_records         |list      |Nested split records (home/away/etc.).                   |
#'  |team_records_records_division_records      |list      |Nested records by division.                              |
#'  |team_records_records_overall_records       |list      |Nested overall records.                                  |
#'  |team_records_records_league_records        |list      |Nested records by league.                                |
#'  |team_records_records_expected_records      |list      |Nested expected (Pythagorean) records.                   |
#'  |league_id                                  |integer   |League MLBAM ID.                                         |
#'  |league_link                                |character |API link to the league.                                  |
#'  |division_id                                |integer   |Division MLBAM ID.                                       |
#'  |division_link                              |character |API link to the division.                                |
#'  |sport_id                                   |integer   |Sport MLBAM ID.                                          |
#'  |sport_link                                 |character |API link to the sport.                                   |
#'  |round_robin_status                         |character |Round robin status indicator.                            |
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
  
  standings <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      
      standings <- jsonlite::fromJSON(jsonlite::toJSON(resp$records), flatten = TRUE) |> 
        tidyr::unnest("teamRecords", names_sep = "_") |> 
        janitor::clean_names() |> 
        as.data.frame() |>
        make_baseballr_data("MLB Standings data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  # split_standings <- purrr::map_df(1:length(standings$teamRecords_records.splitRecords),
  #                                  function(x){
  #                                    standings$teamRecords_records.splitRecords[[x]] |> 
  #                                      tidyr::pivot_wider(names_from='type',names_glue = "{type}_{.value}",values_from = c("wins","losses", "pct"))
  #                                  })
  return(standings)
}


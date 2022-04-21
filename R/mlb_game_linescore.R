#' @rdname mlb_game_linescore
#' @title **Retrieve game linescores for major and minor league games**
#' @param game_pk The unique game_pk identifier for the game
#' @return Returns a tibble with the following columns
#'  |col_name                                  |types     |
#'  |:-----------------------------------------|:---------|
#'  |game_pk                                   |numeric   |
#'  |home_team_id                              |character |
#'  |home_team_name                            |character |
#'  |away_team_id                              |character |
#'  |away_team_name                            |character |
#'  |num                                       |integer   |
#'  |ordinal_num                               |character |
#'  |home_runs                                 |integer   |
#'  |home_hits                                 |integer   |
#'  |home_errors                               |integer   |
#'  |home_left_on_base                         |integer   |
#'  |away_runs                                 |integer   |
#'  |away_hits                                 |integer   |
#'  |away_errors                               |integer   |
#'  |away_left_on_base                         |integer   |
#'  |home_team_link                            |character |
#'  |home_team_season                          |character |
#'  |home_team_venue_id                        |character |
#'  |home_team_venue_name                      |character |
#'  |home_team_venue_link                      |character |
#'  |home_team_team_code                       |character |
#'  |home_team_file_code                       |character |
#'  |home_team_abbreviation                    |character |
#'  |home_team_team_name                       |character |
#'  |home_team_location_name                   |character |
#'  |home_team_first_year_of_play              |character |
#'  |home_team_league_id                       |character |
#'  |home_team_league_name                     |character |
#'  |home_team_league_link                     |character |
#'  |home_team_division_id                     |character |
#'  |home_team_division_name                   |character |
#'  |home_team_division_link                   |character |
#'  |home_team_sport_id                        |character |
#'  |home_team_sport_link                      |character |
#'  |home_team_sport_name                      |character |
#'  |home_team_short_name                      |character |
#'  |home_team_record_games_played             |character |
#'  |home_team_record_wild_card_games_back     |character |
#'  |home_team_record_league_games_back        |character |
#'  |home_team_record_spring_league_games_back |character |
#'  |home_team_record_sport_games_back         |character |
#'  |home_team_record_division_games_back      |character |
#'  |home_team_record_conference_games_back    |character |
#'  |home_team_record_league_record_wins       |character |
#'  |home_team_record_league_record_losses     |character |
#'  |home_team_record_league_record_pct        |character |
#'  |home_team_record_division_leader          |character |
#'  |home_team_record_wins                     |character |
#'  |home_team_record_losses                   |character |
#'  |home_team_record_winning_percentage       |character |
#'  |home_team_franchise_name                  |character |
#'  |home_team_club_name                       |character |
#'  |home_team_all_star_status                 |character |
#'  |home_team_active                          |character |
#'  |away_team_link                            |character |
#'  |away_team_season                          |character |
#'  |away_team_venue_id                        |character |
#'  |away_team_venue_name                      |character |
#'  |away_team_venue_link                      |character |
#'  |away_team_team_code                       |character |
#'  |away_team_file_code                       |character |
#'  |away_team_abbreviation                    |character |
#'  |away_team_team_name                       |character |
#'  |away_team_location_name                   |character |
#'  |away_team_first_year_of_play              |character |
#'  |away_team_league_id                       |character |
#'  |away_team_league_name                     |character |
#'  |away_team_league_link                     |character |
#'  |away_team_division_id                     |character |
#'  |away_team_division_name                   |character |
#'  |away_team_division_link                   |character |
#'  |away_team_sport_id                        |character |
#'  |away_team_sport_link                      |character |
#'  |away_team_sport_name                      |character |
#'  |away_team_short_name                      |character |
#'  |away_team_record_games_played             |character |
#'  |away_team_record_wild_card_games_back     |character |
#'  |away_team_record_league_games_back        |character |
#'  |away_team_record_spring_league_games_back |character |
#'  |away_team_record_sport_games_back         |character |
#'  |away_team_record_division_games_back      |character |
#'  |away_team_record_conference_games_back    |character |
#'  |away_team_record_league_record_wins       |character |
#'  |away_team_record_league_record_losses     |character |
#'  |away_team_record_league_record_pct        |character |
#'  |away_team_record_division_leader          |character |
#'  |away_team_record_wins                     |character |
#'  |away_team_record_losses                   |character |
#'  |away_team_record_winning_percentage       |character |
#'  |away_team_franchise_name                  |character |
#'  |away_team_club_name                       |character |
#'  |away_team_all_star_status                 |character |
#'  |away_team_active                          |character |
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @export
#' @examples \donttest{
#'   try(mlb_game_linescore(game_pk = 566001))
#' }

mlb_game_linescore <- function(game_pk) {
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1.1/game/{game_pk}/feed/live"))
  
  tryCatch(
    expr={
      payload <- mlb_endpoint %>% 
        mlb_api_call() %>% 
        jsonlite::toJSON() %>% 
        jsonlite::fromJSON(flatten = TRUE)
      home_team <- unlist(payload$gameData$teams$home) %>% 
        dplyr::bind_rows() %>% 
        janitor::clean_names() %>%
        dplyr::select(-starts_with("spring_venue"), 
                      -starts_with("spring_league"))
      away_team <- unlist(payload$gameData$teams$away) %>% 
        dplyr::bind_rows() %>% 
        janitor::clean_names() %>%
        dplyr::select(-starts_with("spring_venue"), 
                      -starts_with("spring_league"))
      colnames(home_team) <- paste0("home_team_", colnames(home_team))
      colnames(away_team) <- paste0("away_team_", colnames(away_team))
      
      teams <- home_team %>% 
        dplyr::bind_cols(away_team)
      linescore <- payload$liveData$linescore$innings 
      
      linescore_table <- linescore %>% 
        dplyr::bind_cols(teams) %>% 
        janitor::clean_names() %>% 
        dplyr::mutate(
          game_pk = game_pk) %>% 
        dplyr::select(
          .data$game_pk, 
          .data$home_team_id, .data$home_team_name,
          .data$away_team_id, .data$away_team_name,
          tidyr::everything()) %>%
        make_baseballr_data("MLB Game Linescore data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(linescore_table)
}

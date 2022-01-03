#' @rdname mlb_homerun_derby
#' @title **Retrieve Homerun Derby data**
#'
#' @param game_pk The game_pk for which you want to return data
#' @return Returns a data frame with the following columns 
#'   |col_name                                         |types     |
#'   |:------------------------------------------------|:---------|
#'   |game_pk                                          |integer   |
#'   |event_name                                       |character |
#'   |event_date                                       |character |
#'   |event_type_code                                  |character |
#'   |event_type_name                                  |character |
#'   |venue_id                                         |integer   |
#'   |venue_name                                       |character |
#'   |round                                            |integer   |
#'   |num_batters                                      |integer   |
#'   |batter                                           |character |
#'   |batter_id                                        |integer   |
#'   |batter_link                                      |character |
#'   |top_seed_started                                 |logical   |
#'   |top_seed_complete                                |logical   |
#'   |top_seed_winner                                  |logical   |
#'   |bonus_time                                       |logical   |
#'   |home_run                                         |logical   |
#'   |tie_breaker                                      |logical   |
#'   |is_home_run                                      |logical   |
#'   |time_remaining                                   |character |
#'   |is_bonus_time                                    |logical   |
#'   |is_tie_breaker                                   |logical   |
#'   |hit_data_launch_speed                            |integer   |
#'   |hit_data_launch_angle                            |integer   |
#'   |hit_data_total_distance                          |integer   |
#'   |hit_data_coordinates_coord_x                     |numeric   |
#'   |hit_data_coordinates_coord_y                     |numeric   |
#'   |hit_data_coordinates_landing_pos_x               |numeric   |
#'   |hit_data_coordinates_landing_pos_y               |numeric   |
#'   |hit_data_trajectory_data_trajectory_polynomial_x |list      |
#'   |hit_data_trajectory_data_trajectory_polynomial_y |list      |
#'   |hit_data_trajectory_data_trajectory_polynomial_z |list      |
#'   |hit_data_trajectory_data_valid_time_interval     |list      |
#'   |top_seed_seed                                    |integer   |
#'   |top_seed_is_winner                               |logical   |
#'   |top_seed_is_complete                             |logical   |
#'   |top_seed_is_started                              |logical   |
#'   |top_seed_num_home_runs                           |integer   |
#'   |top_seed_player_id                               |integer   |
#'   |top_seed_player_full_name                        |character |
#'   |top_seed_player_link                             |character |
#'   |top_seed_top_derby_hit_data_launch_speed         |integer   |
#'   |top_seed_top_derby_hit_data_total_distance       |integer   |
#'   |bottom_seed_started                              |logical   |
#'   |bottom_seed_complete                             |logical   |
#'   |bottom_seed_winner                               |logical   |
#'   |bottom_seed_seed                                 |integer   |
#'   |bottom_seed_is_winner                            |logical   |
#'   |bottom_seed_is_complete                          |logical   |
#'   |bottom_seed_is_started                           |logical   |
#'   |bottom_seed_num_home_runs                        |integer   |
#'   |bottom_seed_player_id                            |integer   |
#'   |bottom_seed_player_full_name                     |character |
#'   |bottom_seed_player_link                          |character |
#'   |bottom_seed_top_derby_hit_data_launch_speed      |integer   |
#'   |bottom_seed_top_derby_hit_data_total_distance    |integer   |
#'   |venue_link                                       |character |
#'   |is_multi_day                                     |logical   |
#'   |is_primary_calendar                              |logical   |
#'   |file_code                                        |character |
#'   |event_number                                     |integer   |
#'   |public_facing                                    |logical   | 
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \donttest{
#'  mlb_homerun_derby(game_pk = 511101)
#' }

mlb_homerun_derby <- function(
    game_pk
  ) {
  
  query_params <- list()
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/homeRunDerby/{game_pk}"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE)
  
  resp$info$teams <- NA
  
  info <- resp$info %>% 
     as.data.frame() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE) %>%  
    janitor::clean_names() %>% 
    dplyr::rename(
      game_pk = .data$id,
      event_name = .data$name)
  
  rounds <- resp$rounds %>% 
    as.data.frame() %>% 
    tidyr::unnest(.data$matchups) %>% 
    janitor::clean_names() 
  
  top_seed <- rounds %>% 
    tidyr::unnest(.data$top_seed_hits) %>%
    janitor::clean_names() %>% 
    dplyr::select(-.data$bottom_seed_hits) %>% 
    dplyr::mutate(
      batter = .data$top_seed_player_full_name,
      batter_id = .data$top_seed_player_id,
      batter_link = .data$top_seed_player_link)
  
  bottom_seed <- rounds %>% 
    tidyr::unnest(.data$bottom_seed_hits) %>% 
    janitor::clean_names() %>% 
    dplyr::select(-.data$top_seed_hits) %>% 
    dplyr::mutate(
      batter = .data$bottom_seed_player_full_name,
      batter_id = .data$bottom_seed_player_id,
      batter_link = .data$bottom_seed_player_link)
  
  rounds_hits <- top_seed %>% 
    dplyr::bind_rows(bottom_seed) %>% 
    dplyr::bind_cols(info) %>% 
    dplyr::select(
      .data$game_pk,
      .data$event_name,
      .data$event_date,
      .data$event_type_code,
      .data$event_type_name,
      .data$venue_id,
      .data$venue_name,
      .data$round, 
      .data$num_batters, 
      .data$batter, 
      .data$batter_id, 
      .data$batter_link, 
      tidyr::everything())
  return(rounds_hits)
}
#' @rdname mlb_homerun_derby_bracket
#' @title **Retrieve Homerun Derby Bracket**
#'
#' @param game_pk The game_pk for which you want to return data
#' @return Returns a data frame with the following columns 
#'    |col_name                                      |types     |
#'    |:---------------------------------------------|:---------|
#'    |game_pk                                       |integer   |
#'    |event_name                                    |character |
#'    |event_type_code                               |character |
#'    |event_type_name                               |character |
#'    |event_date                                    |character |
#'    |venue_id                                      |integer   |
#'    |venue_name                                    |character |
#'    |venue_link                                    |character |
#'    |is_multi_day                                  |logical   |
#'    |is_primary_calendar                           |logical   |
#'    |file_code                                     |character |
#'    |event_number                                  |integer   |
#'    |public_facing                                 |logical   |
#'    |round                                         |integer   |
#'    |num_batters                                   |integer   |
#'    |top_seed_complete                             |logical   |
#'    |top_seed_started                              |logical   |
#'    |top_seed_winner                               |logical   |
#'    |top_seed_seed                                 |integer   |
#'    |top_seed_is_winner                            |logical   |
#'    |top_seed_is_complete                          |logical   |
#'    |top_seed_is_started                           |logical   |
#'    |top_seed_num_home_runs                        |integer   |
#'    |top_seed_player_id                            |integer   |
#'    |top_seed_player_full_name                     |character |
#'    |top_seed_player_link                          |character |
#'    |top_seed_top_derby_hit_data_launch_speed      |integer   |
#'    |top_seed_top_derby_hit_data_total_distance    |integer   |
#'    |bottom_seed_complete                          |logical   |
#'    |bottom_seed_started                           |logical   |
#'    |bottom_seed_winner                            |logical   |
#'    |bottom_seed_seed                              |integer   |
#'    |bottom_seed_is_winner                         |logical   |
#'    |bottom_seed_is_complete                       |logical   |
#'    |bottom_seed_is_started                        |logical   |
#'    |bottom_seed_num_home_runs                     |integer   |
#'    |bottom_seed_player_id                         |integer   |
#'    |bottom_seed_player_full_name                  |character |
#'    |bottom_seed_player_link                       |character |
#'    |bottom_seed_top_derby_hit_data_launch_speed   |integer   |
#'    |bottom_seed_top_derby_hit_data_total_distance |integer   |
#'  
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \donttest{
#'  mlb_homerun_derby_bracket(game_pk = 511101)
#' }

mlb_homerun_derby_bracket <- function(
  game_pk
) {
  
  query_params <- list()
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/homeRunDerby/{game_pk}/bracket"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE)
  resp$info$teams <- NA
  info <- resp$info %>% 
    as.data.frame() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE) %>%  
    janitor::clean_names() %>% 
    dplyr::rename(
      game_pk = .data$id,
      event_name = .data$name)
  
  rounds <- resp$rounds %>% 
    as.data.frame() %>% 
    tidyr::unnest(.data$matchups) %>% 
    janitor::clean_names() 
  
  bracket <- info %>% 
    dplyr::bind_cols(rounds) %>% 
    dplyr::select(-.data$top_seed_hits, -.data$bottom_seed_hits)
  
  return(bracket)
}

#' @rdname mlb_homerun_derby_players
#' @title **Retrieve Homerun Derby Players**
#'
#' @param game_pk The game_pk for which you want to return data
#' @return Returns a data frame with the following columns 
#'   |col_name                                       |types     |
#'   |:----------------------------------------------|:---------|
#'   |player_id                                      |integer   |
#'   |player_full_name                               |character |
#'   |player_link                                    |character |
#'   |player_first_name                              |character |
#'   |player_last_name                               |character |
#'   |player_primary_number                          |character |
#'   |player_birth_date                              |character |
#'   |player_current_age                             |integer   |
#'   |player_birth_city                              |character |
#'   |player_birth_state_province                    |character |
#'   |player_birth_country                           |character |
#'   |player_height                                  |character |
#'   |player_weight                                  |integer   |
#'   |player_active                                  |logical   |
#'   |player_use_name                                |character |
#'   |player_middle_name                             |character |
#'   |player_boxscore_name                           |character |
#'   |player_nick_name                               |character |
#'   |player_gender                                  |character |
#'   |player_is_player                               |logical   |
#'   |player_is_verified                             |logical   |
#'   |player_draft_year                              |integer   |
#'   |player_pronunciation                           |character |
#'   |player_mlb_debut_date                          |character |
#'   |player_name_first_last                         |character |
#'   |player_name_slug                               |character |
#'   |player_first_last_name                         |character |
#'   |player_last_first_name                         |character |
#'   |player_last_init_name                          |character |
#'   |player_init_last_name                          |character |
#'   |player_full_fml_name                           |character |
#'   |player_full_lfm_name                           |character |
#'   |player_strike_zone_top                         |numeric   |
#'   |player_strike_zone_bottom                      |numeric   |
#'   |player_name_matrilineal                        |character |
#'   |player_current_team_id                         |integer   |
#'   |player_current_team_name                       |character |
#'   |player_current_team_link                       |character |
#'   |player_current_team_season                     |integer   |
#'   |player_current_team_team_code                  |character |
#'   |player_current_team_file_code                  |character |
#'   |player_current_team_abbreviation               |character |
#'   |player_current_team_team_name                  |character |
#'   |player_current_team_location_name              |character |
#'   |player_current_team_first_year_of_play         |character |
#'   |player_current_team_short_name                 |character |
#'   |player_current_team_franchise_name             |character |
#'   |player_current_team_club_name                  |character |
#'   |player_current_team_all_star_status            |character |
#'   |player_current_team_active                     |logical   |
#'   |player_current_team_parent_org_name            |character |
#'   |player_current_team_parent_org_id              |integer   |
#'   |player_current_team_venue_id                   |integer   |
#'   |player_current_team_venue_name                 |character |
#'   |player_current_team_venue_link                 |character |
#'   |player_current_team_spring_venue_id            |integer   |
#'   |player_current_team_spring_venue_link          |character |
#'   |player_current_team_league_id                  |integer   |
#'   |player_current_team_league_name                |character |
#'   |player_current_team_league_link                |character |
#'   |player_current_team_division_id                |integer   |
#'   |player_current_team_division_name              |character |
#'   |player_current_team_division_link              |character |
#'   |player_current_team_sport_id                   |integer   |
#'   |player_current_team_sport_link                 |character |
#'   |player_current_team_sport_name                 |character |
#'   |player_current_team_spring_league_id           |integer   |
#'   |player_current_team_spring_league_name         |character |
#'   |player_current_team_spring_league_link         |character |
#'   |player_current_team_spring_league_abbreviation |character |
#'   |player_primary_position_code                   |character |
#'   |player_primary_position_name                   |character |
#'   |player_primary_position_type                   |character |
#'   |player_primary_position_abbreviation           |character |
#'   |player_bat_side_code                           |character |
#'   |player_bat_side_description                    |character |
#'   |player_pitch_hand_code                         |character |
#'   |player_pitch_hand_description                  |character |
#'   
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \donttest{
#'  mlb_homerun_derby_players(game_pk = 511101)
#' }

mlb_homerun_derby_players <- function(
  game_pk
) {
  
  query_params <- list()
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/homeRunDerby/{game_pk}/pool"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE)
  resp$info$teams <- NA
  info <- resp$info %>% 
    as.data.frame() %>% 
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON(flatten = TRUE) %>%  
    janitor::clean_names() %>% 
    dplyr::rename(
      game_pk = .data$id,
      event_name = .data$name)
  
  rounds <- resp$rounds %>% 
    as.data.frame() %>% 
    tidyr::unnest(.data$matchups) %>% 
    janitor::clean_names() 
  
  players <- resp$players %>% 
    as.data.frame() %>%  
    janitor::clean_names() %>% 
    dplyr::select(-.data$stats)
  colnames(players) <- paste0("player_", colnames(players))
  return(players)
}


#' @rdname mlb_homerun_derby_endpoints
#' @name mlb_homerun_derby_endpoints
#' @aliases mlb_homerun_derby_endpoints homerun_derby hr_derby
#' @title
#' **MLB Home Run Derby Endpoint Overview**
#' @description
#'
#' * `mlb_homerun_derby()`: Retrieve Homerun Derby data.
#' * `mlb_homerun_derby_bracket()`: Retrieve Homerun Derby Bracket.
#' * `mlb_homerun_derby_players()`: Retrieve Homerun Derby Players.
#'
#' @details
#' ## **MLB Home Run Derby**
#'
#' These functions retrieve MLB Home Run Derby data, bracket, and player information from the MLB Stats API.
#'
#' @family MLB Home Run Derby
NULL

#' @rdname mlb_homerun_derby
#' @title **Retrieve Homerun Derby data**
#'
#' @param game_pk The game_pk for which you want to return data
#' @return Returns a tibble with the following columns
#'
#'   |col_name                                         |types     |description                                                       |
#'   |:------------------------------------------------|:---------|:-----------------------------------------------------------------|
#'   |game_pk                                          |integer   |MLB game primary key for the Home Run Derby event.                |
#'   |event_name                                       |character |Event name (e.g. 'All-Star Workout Day: Home Run Derby').         |
#'   |event_date                                       |character |Event date-time in ISO 8601 (e.g. '2017-07-11T00:00:00Z').        |
#'   |event_type_code                                  |character |Single-letter event type code (e.g. 'O').                         |
#'   |event_type_name                                  |character |Event type name (e.g. 'Other').                                   |
#'   |venue_id                                         |integer   |MLB venue id hosting the event.                                   |
#'   |venue_name                                       |character |Venue name (e.g. 'Marlins Park').                                 |
#'   |round                                            |integer   |Derby round number for the matchup.                               |
#'   |batter                                           |character |Full name of the batter for this swing record.                    |
#'   |batter_id                                        |integer   |MLB player id of the batter.                                      |
#'   |batter_link                                      |character |API relative link to the batter.                                  |
#'   |top_seed_complete                                |logical   |Whether the top seed's turn in the matchup is complete.           |
#'   |top_seed_started                                 |logical   |Whether the top seed's turn in the matchup has started.           |
#'   |top_seed_winner                                  |logical   |Whether the top seed won the matchup.                             |
#'   |bonus_time                                       |logical   |Whether the swing occurred during bonus time.                     |
#'   |home_run                                         |logical   |Whether the swing was scored a home run.                          |
#'   |tie_breaker                                      |logical   |Whether the swing occurred during a tie-breaker.                  |
#'   |is_home_run                                      |logical   |Whether the recorded hit is a home run.                           |
#'   |time_remaining                                   |character |Time remaining on the clock when the swing occurred.              |
#'   |is_bonus_time                                    |logical   |Whether the swing counted toward bonus time.                      |
#'   |is_tie_breaker                                   |logical   |Whether the swing counted toward a tie-breaker.                   |
#'   |hit_data_launch_speed                            |integer   |Exit velocity of the home run swing (mph).                        |
#'   |hit_data_launch_angle                            |integer   |Launch angle of the batted ball (degrees).                        |
#'   |hit_data_total_distance                          |integer   |Projected total distance of the batted ball (feet).               |
#'   |hit_data_coordinates_coord_x                     |numeric   |Hit location x-coordinate on the field overlay.                   |
#'   |hit_data_coordinates_coord_y                     |numeric   |Hit location y-coordinate on the field overlay.                   |
#'   |hit_data_coordinates_landing_pos_x               |numeric   |Landing position x-coordinate of the batted ball.                 |
#'   |hit_data_coordinates_landing_pos_y               |numeric   |Landing position y-coordinate of the batted ball.                 |
#'   |hit_data_trajectory_data_trajectory_polynomial_x |list      |Polynomial coefficients of the x trajectory.                      |
#'   |hit_data_trajectory_data_trajectory_polynomial_y |list      |Polynomial coefficients of the y trajectory.                      |
#'   |hit_data_trajectory_data_trajectory_polynomial_z |list      |Polynomial coefficients of the z trajectory.                      |
#'   |hit_data_trajectory_data_valid_time_interval     |list      |Valid time interval for the trajectory fit (seconds).             |
#'   |top_seed_seed                                    |integer   |Bracket seed number of the top seed.                              |
#'   |top_seed_is_winner                               |logical   |Whether the top seed is the matchup winner.                       |
#'   |top_seed_is_complete                             |logical   |Whether the top seed's turn is complete.                          |
#'   |top_seed_is_started                              |logical   |Whether the top seed's turn has started.                          |
#'   |top_seed_num_home_runs                           |integer   |Number of home runs hit by the top seed.                          |
#'   |top_seed_player_id                               |integer   |MLB player id of the top seed.                                    |
#'   |top_seed_player_full_name                        |character |Full name of the top seed.                                        |
#'   |top_seed_player_link                             |character |API relative link to the top seed player.                         |
#'   |top_seed_top_derby_hit_data_launch_speed         |integer   |Top seed's hardest-hit exit velocity in the round (mph).          |
#'   |top_seed_top_derby_hit_data_total_distance       |integer   |Top seed's longest projected distance in the round (feet).        |
#'   |bottom_seed_complete                             |logical   |Whether the bottom seed's turn in the matchup is complete.        |
#'   |bottom_seed_started                              |logical   |Whether the bottom seed's turn in the matchup has started.        |
#'   |bottom_seed_winner                               |logical   |Whether the bottom seed won the matchup.                          |
#'   |bottom_seed_seed                                 |integer   |Bracket seed number of the bottom seed.                           |
#'   |bottom_seed_is_winner                            |logical   |Whether the bottom seed is the matchup winner.                    |
#'   |bottom_seed_is_complete                          |logical   |Whether the bottom seed's turn is complete.                       |
#'   |bottom_seed_is_started                           |logical   |Whether the bottom seed's turn has started.                       |
#'   |bottom_seed_num_home_runs                        |integer   |Number of home runs hit by the bottom seed.                       |
#'   |bottom_seed_player_id                            |integer   |MLB player id of the bottom seed.                                 |
#'   |bottom_seed_player_full_name                     |character |Full name of the bottom seed.                                     |
#'   |bottom_seed_player_link                          |character |API relative link to the bottom seed player.                      |
#'   |bottom_seed_top_derby_hit_data_launch_speed      |integer   |Bottom seed's hardest-hit exit velocity in the round (mph).       |
#'   |bottom_seed_top_derby_hit_data_total_distance    |integer   |Bottom seed's longest projected distance in the round (feet).     |
#'   |venue_link                                       |character |API relative link to the event venue.                             |
#'   |is_multi_day                                     |logical   |Whether the event spans multiple days.                            |
#'   |is_primary_calendar                              |logical   |Whether the event is on the primary calendar.                     |
#'   |file_code                                        |character |Internal file code for the event.                                 |
#'   |event_number                                     |integer   |Event number identifier.                                          |
#'   |public_facing                                    |logical   |Whether the event is public facing.                               |
#'
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \donttest{
#'   try(mlb_homerun_derby(game_pk = 511101))
#' }

mlb_homerun_derby <- function(game_pk){
  
  query_params <- list()
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/homeRunDerby/{game_pk}"))
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  rounds_hits <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call() |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE)
      
      resp$info$teams <- NA
      
      info <- resp$info |> 
        as.data.frame() |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE) |>  
        janitor::clean_names() |> 
        dplyr::rename(
          "game_pk" = "id",
          "event_name" = "name")
      
      rounds <- resp$rounds |> 
        as.data.frame() |> 
        tidyr::unnest("matchups") |> 
        janitor::clean_names() 
      
      top_seed <- rounds |> 
        tidyr::unnest("top_seed_hits") |>
        janitor::clean_names() |> 
        dplyr::select(-dplyr::any_of("bottom_seed_hits")) |> 
        dplyr::mutate(
          batter = .data$top_seed_player_full_name,
          batter_id = .data$top_seed_player_id,
          batter_link = .data$top_seed_player_link)
      
      bottom_seed <- rounds |> 
        tidyr::unnest("bottom_seed_hits") |> 
        janitor::clean_names() |> 
        dplyr::select(-dplyr::any_of("top_seed_hits")) |> 
        dplyr::mutate(
          batter = .data$bottom_seed_player_full_name,
          batter_id = .data$bottom_seed_player_id,
          batter_link = .data$bottom_seed_player_link)
      
      rounds_hits <- top_seed |> 
        dplyr::bind_rows(bottom_seed) |> 
        dplyr::bind_cols(info) |> 
        dplyr::select(
          dplyr::any_of(c(
            "game_pk",
            "event_name",
            "event_date",
            "event_type_code",
            "event_type_name",
            "venue_id",
            "venue_name",
            "round",
            "num_batters",
            "batter",
            "batter_id",
            "batter_link")),
          tidyr::everything()) |>
        dplyr::select(dplyr::any_of(c(
          "game_pk", "event_name", "event_date",
          "event_type_code", "event_type_name",
          "venue_id", "venue_name", "round",
          "num_batters", "batter", "batter_id",
          "batter_link", "top_seed_complete", 
          "top_seed_started", "top_seed_winner",
          "bonus_time", "home_run", "tie_breaker",
          "is_home_run", "time_remaining",
          "is_bonus_time", "is_tie_breaker", 
          "hit_data_launch_speed", "hit_data_launch_angle",
          "hit_data_total_distance", "hit_data_coordinates_coord_x",
          "hit_data_coordinates_coord_y", "hit_data_coordinates_landing_pos_x",
          "hit_data_coordinates_landing_pos_y", 
          "hit_data_trajectory_data_trajectory_polynomial_x", 
          "hit_data_trajectory_data_trajectory_polynomial_y",
          "hit_data_trajectory_data_trajectory_polynomial_z", 
          "hit_data_trajectory_data_valid_time_interval",
          "top_seed_seed", "top_seed_is_winner", "top_seed_is_complete",
          "top_seed_is_started", "top_seed_num_home_runs", 
          "top_seed_player_id", "top_seed_player_full_name",
          "top_seed_player_link", "top_seed_top_derby_hit_data_launch_speed",
          "top_seed_top_derby_hit_data_total_distance", "bottom_seed_complete",
          "bottom_seed_started", "bottom_seed_winner", "bottom_seed_seed",
          "bottom_seed_is_winner", "bottom_seed_is_complete",
          "bottom_seed_is_started", "bottom_seed_num_home_runs", 
          "bottom_seed_player_id", "bottom_seed_player_full_name", 
          "bottom_seed_player_link", "bottom_seed_top_derby_hit_data_launch_speed",
          "bottom_seed_top_derby_hit_data_total_distance",
          "venue_link", "is_multi_day",
          "is_primary_calendar", "file_code", "event_number",
          "public_facing"))) |>
        make_baseballr_data("MLB Homerun Derby data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(rounds_hits)
}
#' @rdname mlb_homerun_derby_bracket
#' @title **Retrieve Homerun Derby Bracket**
#'
#' @param game_pk The game_pk for which you want to return data
#' @return Returns a tibble with the following columns
#'
#'    |col_name                                      |types     |description                                                   |
#'    |:---------------------------------------------|:---------|:-------------------------------------------------------------|
#'    |game_pk                                       |integer   |MLB game primary key for the Home Run Derby event.            |
#'    |event_name                                    |character |Event name (e.g. 'All-Star Workout Day: Home Run Derby').     |
#'    |event_type_code                               |character |Single-letter event type code (e.g. 'O').                     |
#'    |event_type_name                               |character |Event type name (e.g. 'Other').                               |
#'    |event_date                                    |character |Event date-time in ISO 8601 (e.g. '2017-07-11T00:00:00Z').    |
#'    |venue_id                                      |integer   |MLB venue id hosting the event.                               |
#'    |venue_name                                    |character |Venue name (e.g. 'Marlins Park').                             |
#'    |venue_link                                    |character |API relative link to the event venue.                         |
#'    |is_multi_day                                  |logical   |Whether the event spans multiple days.                        |
#'    |is_primary_calendar                           |logical   |Whether the event is on the primary calendar.                 |
#'    |file_code                                     |character |Internal file code for the event.                             |
#'    |event_number                                  |integer   |Event number identifier.                                      |
#'    |public_facing                                 |logical   |Whether the event is public facing.                           |
#'    |round                                         |integer   |Derby bracket round number.                                   |
#'    |top_seed_complete                             |logical   |Whether the top seed's turn in the matchup is complete.       |
#'    |top_seed_started                              |logical   |Whether the top seed's turn in the matchup has started.       |
#'    |top_seed_winner                               |logical   |Whether the top seed won the matchup.                         |
#'    |top_seed_seed                                 |integer   |Bracket seed number of the top seed.                          |
#'    |top_seed_is_winner                            |logical   |Whether the top seed is the matchup winner.                   |
#'    |top_seed_is_complete                          |logical   |Whether the top seed's turn is complete.                      |
#'    |top_seed_is_started                           |logical   |Whether the top seed's turn has started.                      |
#'    |top_seed_num_home_runs                        |integer   |Number of home runs hit by the top seed.                      |
#'    |top_seed_player_id                            |integer   |MLB player id of the top seed.                                |
#'    |top_seed_player_full_name                     |character |Full name of the top seed.                                    |
#'    |top_seed_player_link                          |character |API relative link to the top seed player.                     |
#'    |top_seed_top_derby_hit_data_launch_speed      |integer   |Top seed's hardest-hit exit velocity in the round (mph).      |
#'    |top_seed_top_derby_hit_data_total_distance    |integer   |Top seed's longest projected distance in the round (feet).    |
#'    |bottom_seed_complete                          |logical   |Whether the bottom seed's turn in the matchup is complete.    |
#'    |bottom_seed_started                           |logical   |Whether the bottom seed's turn in the matchup has started.    |
#'    |bottom_seed_winner                            |logical   |Whether the bottom seed won the matchup.                      |
#'    |bottom_seed_seed                              |integer   |Bracket seed number of the bottom seed.                       |
#'    |bottom_seed_is_winner                         |logical   |Whether the bottom seed is the matchup winner.                |
#'    |bottom_seed_is_complete                       |logical   |Whether the bottom seed's turn is complete.                   |
#'    |bottom_seed_is_started                        |logical   |Whether the bottom seed's turn has started.                   |
#'    |bottom_seed_num_home_runs                     |integer   |Number of home runs hit by the bottom seed.                   |
#'    |bottom_seed_player_id                         |integer   |MLB player id of the bottom seed.                             |
#'    |bottom_seed_player_full_name                  |character |Full name of the bottom seed.                                 |
#'    |bottom_seed_player_link                       |character |API relative link to the bottom seed player.                  |
#'    |bottom_seed_top_derby_hit_data_launch_speed   |integer   |Bottom seed's hardest-hit exit velocity in the round (mph).   |
#'    |bottom_seed_top_derby_hit_data_total_distance |integer   |Bottom seed's longest projected distance in the round (feet). |
#'
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \donttest{
#'   try(mlb_homerun_derby_bracket(game_pk = 511101))
#' }

mlb_homerun_derby_bracket <- function(game_pk){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/homeRunDerby/{game_pk}/bracket"))
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  bracket <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call() |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE)
      resp$info$teams <- NA
      info <- resp$info |> 
        as.data.frame() |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE) |>  
        janitor::clean_names() |> 
        dplyr::rename(
          "game_pk" = "id",
          "event_name" = "name")
      
      rounds <- resp$rounds |> 
        as.data.frame() |> 
        tidyr::unnest("matchups") |> 
        janitor::clean_names() 
      
      bracket <- info |> 
        dplyr::bind_cols(rounds) |> 
        dplyr::select(-dplyr::any_of("top_seed_hits"), -dplyr::any_of("bottom_seed_hits")) |>
        dplyr::select(dplyr::any_of(c(
          "game_pk", "event_name", "event_type_code",
          "event_type_name", "event_date", "venue_id",
          "venue_name", "venue_link", "is_multi_day",
          "is_primary_calendar", "file_code", "event_number",
          "public_facing", "round", "num_batters",
          "top_seed_complete", "top_seed_started", 
          "top_seed_winner", "top_seed_seed", 
          "top_seed_is_winner", "top_seed_is_complete",
          "top_seed_is_started", "top_seed_num_home_runs",
          "top_seed_player_id", "top_seed_player_full_name", 
          "top_seed_player_link", "top_seed_top_derby_hit_data_launch_speed", 
          "top_seed_top_derby_hit_data_total_distance", "bottom_seed_complete",
          "bottom_seed_started", "bottom_seed_winner", "bottom_seed_seed",
          "bottom_seed_is_winner", "bottom_seed_is_complete",
          "bottom_seed_is_started", "bottom_seed_num_home_runs", 
          "bottom_seed_player_id", "bottom_seed_player_full_name",
          "bottom_seed_player_link", "bottom_seed_top_derby_hit_data_launch_speed",
          "bottom_seed_top_derby_hit_data_total_distance"
        ))) |>
        make_baseballr_data("MLB Homerun Derby Bracket data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(bracket)
}

#' @rdname mlb_homerun_derby_players
#' @title **Retrieve Homerun Derby Players**
#'
#' @param game_pk The game_pk for which you want to return data
#' @return Returns a tibble with the following columns 
#'
#'   |col_name                                       |types     |description                                                       |
#'   |:----------------------------------------------|:---------|:-----------------------------------------------------------------|
#'   |game_pk                                        |integer   |MLB game primary key for the Home Run Derby event.                |
#'   |event_name                                     |character |Event name (e.g. 'All-Star Workout Day: Home Run Derby').         |
#'   |event_date                                     |character |Event date-time in ISO 8601 (e.g. '2017-07-11T00:00:00Z').        |
#'   |event_type_code                                |character |Single-letter event type code (e.g. 'O').                        |
#'   |event_type_name                                |character |Event type name (e.g. 'Other').                                   |
#'   |venue_id                                       |integer   |MLB venue id hosting the event.                                   |
#'   |venue_name                                     |character |Venue name (e.g. 'Marlins Park').                                 |
#'   |player_id                                      |integer   |MLB player id of the participant.                                 |
#'   |player_full_name                               |character |Participant full name.                                            |
#'   |player_link                                    |character |API relative link to the player.                                  |
#'   |player_first_name                              |character |Participant first name.                                           |
#'   |player_last_name                               |character |Participant last name.                                            |
#'   |player_primary_number                          |character |Participant primary jersey number.                                |
#'   |player_birth_date                              |character |Participant birth date (YYYY-MM-DD).                              |
#'   |player_current_age                             |integer   |Participant current age in years.                                 |
#'   |player_birth_city                              |character |Participant birth city.                                           |
#'   |player_birth_state_province                    |character |Participant birth state or province.                              |
#'   |player_birth_country                           |character |Participant birth country.                                        |
#'   |player_height                                  |character |Participant height (e.g. "6' 5\"").                              |
#'   |player_weight                                  |integer   |Participant weight in pounds.                                     |
#'   |player_active                                  |logical   |Whether the participant is currently an active player.            |
#'   |player_use_name                                |character |Participant preferred display first name.                         |
#'   |player_middle_name                             |character |Participant middle name.                                          |
#'   |player_boxscore_name                           |character |Participant short box score name.                                 |
#'   |player_nick_name                               |character |Participant nickname.                                             |
#'   |player_gender                                  |character |Participant gender code (e.g. 'M').                              |
#'   |player_is_player                               |logical   |Whether the person is classified as a player.                     |
#'   |player_is_verified                             |logical   |Whether the player profile is verified.                           |
#'   |player_draft_year                              |integer   |Year the participant was drafted.                                 |
#'   |player_pronunciation                           |character |Phonetic pronunciation of the participant's name.                 |
#'   |player_mlb_debut_date                          |character |Participant MLB debut date (YYYY-MM-DD).                          |
#'   |player_name_first_last                         |character |Participant name in first-last order.                             |
#'   |player_name_slug                               |character |URL slug for the participant (name plus id).                      |
#'   |player_first_last_name                         |character |Participant name in first-last order.                             |
#'   |player_last_first_name                         |character |Participant name in last, first order.                            |
#'   |player_last_init_name                          |character |Participant name as last, first initial.                          |
#'   |player_init_last_name                          |character |Participant name as first initial last.                           |
#'   |player_full_fml_name                           |character |Participant full first-middle-last name.                          |
#'   |player_full_lfm_name                           |character |Participant full last, first-middle name.                         |
#'   |player_strike_zone_top                         |numeric   |Top of the participant's strike zone (feet).                      |
#'   |player_strike_zone_bottom                      |numeric   |Bottom of the participant's strike zone (feet).                   |
#'   |player_name_matrilineal                        |character |Participant matrilineal name when provided.                       |
#'   |player_current_team_id                         |integer   |Participant current team id.                                      |
#'   |player_current_team_name                       |character |Participant current team name.                                    |
#'   |player_current_team_link                       |character |API relative link to the current team.                            |
#'   |player_current_team_season                     |integer   |Season of the current team reference.                             |
#'   |player_current_team_team_code                  |character |Current team three-letter team code.                              |
#'   |player_current_team_file_code                  |character |Current team file code.                                           |
#'   |player_current_team_abbreviation               |character |Current team abbreviation (e.g. 'NYY').                          |
#'   |player_current_team_team_name                  |character |Current team short team name (e.g. 'Yankees').                    |
#'   |player_current_team_location_name              |character |Current team location name.                                       |
#'   |player_current_team_first_year_of_play         |character |Franchise first year of play.                                     |
#'   |player_current_team_short_name                 |character |Current team short name.                                          |
#'   |player_current_team_franchise_name             |character |Current team franchise name.                                      |
#'   |player_current_team_club_name                  |character |Current team club name.                                           |
#'   |player_current_team_all_star_status            |character |Current team all-star status flag.                                |
#'   |player_current_team_active                     |logical   |Whether the current team is active.                               |
#'   |player_current_team_parent_org_name            |character |Parent organization name (minors).                                |
#'   |player_current_team_parent_org_id              |integer   |Parent organization id (minors).                                  |
#'   |player_current_team_venue_id                   |integer   |Current team home venue id.                                       |
#'   |player_current_team_venue_name                 |character |Current team home venue name.                                     |
#'   |player_current_team_venue_link                 |character |API relative link to the team venue.                              |
#'   |player_current_team_spring_venue_id            |integer   |Current team spring training venue id.                            |
#'   |player_current_team_spring_venue_link          |character |API relative link to the spring venue.                            |
#'   |player_current_team_league_id                  |integer   |Current team league id (e.g. 103, 104).                          |
#'   |player_current_team_league_name                |character |Current team league name.                                         |
#'   |player_current_team_league_link                |character |API relative link to the league.                                  |
#'   |player_current_team_division_id                |integer   |Current team division id.                                         |
#'   |player_current_team_division_name              |character |Current team division name.                                       |
#'   |player_current_team_division_link              |character |API relative link to the division.                                |
#'   |player_current_team_sport_id                   |integer   |Current team sport id (1 for MLB).                                |
#'   |player_current_team_sport_link                 |character |API relative link to the sport.                                   |
#'   |player_current_team_sport_name                 |character |Current team sport name.                                          |
#'   |player_current_team_spring_league_id           |integer   |Spring league id.                                                 |
#'   |player_current_team_spring_league_name         |character |Spring league name (e.g. 'Grapefruit League').                    |
#'   |player_current_team_spring_league_link         |character |API relative link to the spring league.                           |
#'   |player_current_team_spring_league_abbreviation |character |Spring league abbreviation (e.g. 'GL').                          |
#'   |player_primary_position_code                   |character |Participant primary position code.                                |
#'   |player_primary_position_name                   |character |Participant primary position name.                                |
#'   |player_primary_position_type                   |character |Participant primary position type (e.g. 'Hitter').               |
#'   |player_primary_position_abbreviation           |character |Participant primary position abbreviation (e.g. 'DH').           |
#'   |player_bat_side_code                           |character |Participant batting side code (e.g. 'R').                        |
#'   |player_bat_side_description                    |character |Participant batting side description (e.g. 'Right').              |
#'   |player_pitch_hand_code                         |character |Participant throwing hand code (e.g. 'R').                       |
#'   |player_pitch_hand_description                  |character |Participant throwing hand description (e.g. 'Right').            |
#'   |venue_link                                     |character |API relative link to the event venue.                             |
#'   |is_multi_day                                   |logical   |Whether the event spans multiple days.                            |
#'   |is_primary_calendar                            |logical   |Whether the event is on the primary calendar.                     |
#'   |file_code                                      |character |Internal file code for the event.                                 |
#'   |event_number                                   |integer   |Event number identifier.                                          |
#'   |public_facing                                  |logical   |Whether the event is public facing.                               |
#'   
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \donttest{
#'   try(mlb_homerun_derby_players(game_pk = 511101))
#' }

mlb_homerun_derby_players <- function(game_pk){
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/homeRunDerby/{game_pk}/pool"))
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  players <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call() |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE)
      resp$info$teams <- NA
      info <- resp$info |> 
        as.data.frame() |> 
        jsonlite::toJSON() |> 
        jsonlite::fromJSON(flatten = TRUE) |>  
        janitor::clean_names() |> 
        dplyr::rename(
          "game_pk" = "id",
          "event_name" = "name")
      
      players <- resp$players |> 
        as.data.frame() |>  
        janitor::clean_names() |> 
        dplyr::select(-dplyr::any_of("stats"))
      colnames(players) <- paste0("player_", colnames(players))
      
      players <- players |> 
        dplyr::bind_cols(info) |> 
        dplyr::select(
          "game_pk",
          "event_name",
          "event_date",
          "event_type_code",
          "event_type_name",
          "venue_id",
          "venue_name",
          tidyr::everything()) |> 
        dplyr::select(c(
          "game_pk", "event_name", "event_date", 
          "event_type_code", "event_type_name", 
          "venue_id", "venue_name", "player_id",
          "player_full_name", "player_link",
          "player_first_name", "player_last_name", 
          "player_primary_number", "player_birth_date",
          "player_current_age", "player_birth_city", 
          "player_birth_state_province", "player_birth_country",
          "player_height", "player_weight", "player_active", 
          "player_use_name", "player_middle_name", "player_boxscore_name",
          "player_nick_name", "player_gender", "player_is_player", 
          "player_is_verified", "player_draft_year", "player_pronunciation",
          "player_mlb_debut_date", "player_name_first_last", 
          "player_name_slug", "player_first_last_name",
          "player_last_first_name", "player_last_init_name", 
          "player_init_last_name", "player_full_fml_name", 
          "player_full_lfm_name", "player_strike_zone_top", 
          "player_strike_zone_bottom", "player_name_matrilineal",
          "player_current_team_id", "player_current_team_name", 
          "player_current_team_link", "player_current_team_season",
          "player_current_team_team_code", "player_current_team_file_code", 
          "player_current_team_abbreviation", "player_current_team_team_name",
          "player_current_team_location_name",
          "player_current_team_first_year_of_play", 
          "player_current_team_short_name", 
          "player_current_team_franchise_name",
          "player_current_team_club_name", 
          "player_current_team_all_star_status",
          "player_current_team_active", 
          "player_current_team_parent_org_name", 
          "player_current_team_parent_org_id",
          "player_current_team_venue_id", 
          "player_current_team_venue_name", 
          "player_current_team_venue_link", 
          "player_current_team_spring_venue_id",
          "player_current_team_spring_venue_link",
          "player_current_team_league_id", 
          "player_current_team_league_name", 
          "player_current_team_league_link", 
          "player_current_team_division_id",
          "player_current_team_division_name",
          "player_current_team_division_link",
          "player_current_team_sport_id",
          "player_current_team_sport_link", 
          "player_current_team_sport_name",
          "player_current_team_spring_league_id",
          "player_current_team_spring_league_name", 
          "player_current_team_spring_league_link",
          "player_current_team_spring_league_abbreviation", 
          "player_primary_position_code", "player_primary_position_name", 
          "player_primary_position_type", "player_primary_position_abbreviation",
          "player_bat_side_code", "player_bat_side_description",
          "player_pitch_hand_code", "player_pitch_hand_description",
          "venue_link", "is_multi_day", 
          "is_primary_calendar", "file_code", "event_number", "public_facing"
        )) |>
        make_baseballr_data("MLB Homerun Derby Players data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(players)
}


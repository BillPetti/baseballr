#' @title **Find MLB Player Game Stats - Current Game**
#' @param person_id MLBAMIDs for player of interest.
#' @return Returns a tibble with the following columns:
#'
#'   |col_name                      |types     |
#'   |:-----------------------------|:---------|
#'   |type                          |character |
#'   |group                         |character |
#'   |stat_assists                  |integer   |
#'   |stat_put_outs                 |integer   |
#'   |stat_errors                   |integer   |
#'   |stat_chances                  |integer   |
#'   |stat_fielding                 |character |
#'   |stat_caught_stealing          |integer   |
#'   |stat_passed_ball              |integer   |
#'   |stat_stolen_bases             |integer   |
#'   |stat_stolen_base_percentage   |character |
#'   |stat_pickoffs                 |integer   |
#'   |stat_games_played             |integer   |
#'   |stat_games_started            |integer   |
#'   |stat_fly_outs                 |integer   |
#'   |stat_ground_outs              |integer   |
#'   |stat_air_outs                 |integer   |
#'   |stat_runs                     |integer   |
#'   |stat_doubles                  |integer   |
#'   |stat_triples                  |integer   |
#'   |stat_home_runs                |integer   |
#'   |stat_strike_outs              |integer   |
#'   |stat_base_on_balls            |integer   |
#'   |stat_intentional_walks        |integer   |
#'   |stat_hits                     |integer   |
#'   |stat_hit_by_pitch             |integer   |
#'   |stat_at_bats                  |integer   |
#'   |stat_number_of_pitches        |integer   |
#'   |stat_innings_pitched          |character |
#'   |stat_wins                     |integer   |
#'   |stat_losses                   |integer   |
#'   |stat_saves                    |integer   |
#'   |stat_save_opportunities       |integer   |
#'   |stat_holds                    |integer   |
#'   |stat_blown_saves              |integer   |
#'   |stat_earned_runs              |integer   |
#'   |stat_batters_faced            |integer   |
#'   |stat_outs                     |integer   |
#'   |stat_games_pitched            |integer   |
#'   |stat_complete_games           |integer   |
#'   |stat_shutouts                 |integer   |
#'   |stat_pitches_thrown           |integer   |
#'   |stat_balls                    |integer   |
#'   |stat_strikes                  |integer   |
#'   |stat_strike_percentage        |character |
#'   |stat_hit_batsmen              |integer   |
#'   |stat_balks                    |integer   |
#'   |stat_wild_pitches             |integer   |
#'   |stat_rbi                      |integer   |
#'   |stat_games_finished           |integer   |
#'   |stat_runs_scored_per9         |character |
#'   |stat_home_runs_per9           |character |
#'   |stat_inherited_runners        |integer   |
#'   |stat_inherited_runners_scored |integer   |
#'   |stat_catchers_interference    |integer   |
#'   |stat_sac_bunts                |integer   |
#'   |stat_sac_flies                |integer   |
#'   |stat_ground_into_double_play  |integer   |
#'   |stat_ground_into_triple_play  |integer   |
#'   |stat_plate_appearances        |integer   |
#'   |stat_total_bases              |integer   |
#'   |stat_left_on_base             |integer   |
#'   |stat_at_bats_per_home_run     |character |
#'   |game_type                     |character |
#'   |num_teams                     |integer   |
#'   |stat_avg                      |character |
#'   |stat_obp                      |character |
#'   |stat_slg                      |character |
#'   |stat_ops                      |character |
#'   |stat_outs_pitched             |integer   |
#'   |stat_whip                     |character |
#'   |stat_ground_outs_to_airouts   |character |
#'   |stat_pitches_per_inning       |character |
#'   |stat_strikeout_walk_ratio     |character |
#'   |stat_strikeouts_per9inn       |character |
#'   |stat_walks_per9inn            |character |
#'   |stat_hits_per9inn             |character |
#'   |team_id                       |integer   |
#'   |team_name                     |character |
#'   |team_link                     |character |
#'   |opponent_id                   |integer   |
#'   |opponent_name                 |character |
#'   |opponent_link                 |character |
#'   |pitcher_id                    |integer   |
#'   |pitcher_full_name             |character |
#'   |pitcher_link                  |character |
#'   |pitcher_first_name            |character |
#'   |pitcher_last_name             |character |
#'   |batter_id                     |integer   |
#'   |batter_full_name              |character |
#'   |batter_link                   |character |
#'   |batter_first_name             |character |
#'   |batter_last_name              |character |
#'   |total_splits                  |integer   |
#'   |type_display_name             |character |
#'   |group_display_name            |character |
#'   |player_id                     |numeric   |
#'   |game_pk                       |numeric   |
#' @export
#' @examples \donttest{
#'   try(mlb_player_game_stats_current(person_id = 660271))
#' }
mlb_player_game_stats_current <- function(person_id = NULL){
  
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/people/{person_id}/stats/game/current"))
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>%
        mlb_api_call()
      stats <- jsonlite::fromJSON(jsonlite::toJSON(resp$stats), flatten=TRUE) %>% 
        as.data.frame() %>%
        make_baseballr_data("MLB Player Game Stats - Current Game data from MLB.com",Sys.time())
      if(length(stats)>0){
        stats <- stats %>%
          tidyr::unnest(.data$splits) %>% 
          janitor::clean_names() %>% 
          as.data.frame() %>% 
          dplyr::select(-.data$exemptions) %>% 
          dplyr::mutate(
            player_id = person_id) %>%
          make_baseballr_data("MLB Player Game Stats - Current Game data from MLB.com",Sys.time())
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(stats)
}

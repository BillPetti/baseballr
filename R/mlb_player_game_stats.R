#' @title **Find MLB Player Game Stats**
#' @param person_id MLBAMIDs for player of interest.
#' @param game_pk The game_pk to return game_log statistics for a specific player in a specific game and to complete the call.
#' @return Returns a tibble with the following columns:
#'   |col_name                 |types     |
#'   |:------------------------|:---------|
#'   |type                     |character |
#'   |group                    |character |
#'   |assists                  |integer   |
#'   |put_outs                 |integer   |
#'   |errors                   |integer   |
#'   |chances                  |integer   |
#'   |fielding                 |character |
#'   |caught_stealing          |integer   |
#'   |passed_ball              |integer   |
#'   |stolen_bases             |integer   |
#'   |stolen_base_percentage   |character |
#'   |pickoffs                 |integer   |
#'   |games_played             |integer   |
#'   |games_started            |integer   |
#'   |fly_outs                 |integer   |
#'   |ground_outs              |integer   |
#'   |air_outs                 |integer   |
#'   |runs                     |integer   |
#'   |doubles                  |integer   |
#'   |triples                  |integer   |
#'   |home_runs                |integer   |
#'   |strike_outs              |integer   |
#'   |base_on_balls            |integer   |
#'   |intentional_walks        |integer   |
#'   |hits                     |integer   |
#'   |hit_by_pitch             |integer   |
#'   |at_bats                  |integer   |
#'   |number_of_pitches        |integer   |
#'   |innings_pitched          |character |
#'   |wins                     |integer   |
#'   |losses                   |integer   |
#'   |saves                    |integer   |
#'   |save_opportunities       |integer   |
#'   |holds                    |integer   |
#'   |blown_saves              |integer   |
#'   |earned_runs              |integer   |
#'   |batters_faced            |integer   |
#'   |outs                     |integer   |
#'   |games_pitched            |integer   |
#'   |complete_games           |integer   |
#'   |shutouts                 |integer   |
#'   |pitches_thrown           |integer   |
#'   |balls                    |integer   |
#'   |strikes                  |integer   |
#'   |strike_percentage        |character |
#'   |hit_batsmen              |integer   |
#'   |balks                    |integer   |
#'   |wild_pitches             |integer   |
#'   |rbi                      |integer   |
#'   |games_finished           |integer   |
#'   |runs_scored_per9         |character |
#'   |home_runs_per9           |character |
#'   |inherited_runners        |integer   |
#'   |inherited_runners_scored |integer   |
#'   |catchers_interference    |integer   |
#'   |sac_bunts                |integer   |
#'   |sac_flies                |integer   |
#'   |ground_into_double_play  |integer   |
#'   |ground_into_triple_play  |integer   |
#'   |plate_appearances        |integer   |
#'   |total_bases              |integer   |
#'   |left_on_base             |integer   |
#'   |at_bats_per_home_run     |character |
#'   |game_type                |character |
#'   |num_teams                |integer   |
#'   |avg                      |character |
#'   |obp                      |character |
#'   |slg                      |character |
#'   |ops                      |character |
#'   |outs_pitched             |integer   |
#'   |whip                     |character |
#'   |ground_outs_to_airouts   |character |
#'   |pitches_per_inning       |character |
#'   |strikeout_walk_ratio     |character |
#'   |strikeouts_per9inn       |character |
#'   |walks_per9inn            |character |
#'   |hits_per9inn             |character |
#'   |team_id                  |integer   |
#'   |team_name                |character |
#'   |team_link                |character |
#'   |opponent_id              |integer   |
#'   |opponent_name            |character |
#'   |opponent_link            |character |
#'   |pitcher_id               |integer   |
#'   |pitcher_full_name        |character |
#'   |pitcher_link             |character |
#'   |pitcher_first_name       |character |
#'   |pitcher_last_name        |character |
#'   |batter_id                |integer   |
#'   |batter_full_name         |character |
#'   |batter_link              |character |
#'   |batter_first_name        |character |
#'   |batter_last_name         |character |
#'   |total_splits             |integer   |
#'   |type_display_name        |character |
#'   |group_display_name       |character |
#'   |player_id                |numeric   |
#'   |game_pk                  |numeric   |
#' @export
#' @examples \donttest{
#'  try(mlb_player_game_stats(person_id = 605151, game_pk = 531368))
#' }
mlb_player_game_stats <- function(person_id = NULL,
                                  game_pk = NULL){
  
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/people/{person_id}/stats/game/{game_pk}"))
  query_params <- list()
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>%
        mlb_api_call()
      stats <- jsonlite::fromJSON(jsonlite::toJSON(resp$stats), flatten=TRUE) %>%
        tidyr::unnest(.data$splits) %>% 
        janitor::clean_names() %>% 
        as.data.frame() %>% 
        dplyr::select(-.data$exemptions) %>% 
        dplyr::mutate(
          player_id = person_id,
          game_pk = game_pk
        )
      colnames(stats)<-gsub("stat_", "", colnames(stats)) 
      
      stats <- stats %>%
        make_baseballr_data("MLB Player Game Stats data from MLB.com",Sys.time())
      
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


#' @title **MLB Stats**
#' @param stat_type Stat type to return statistics for.
#' @param player_pool There are 4 different types of player pools to return statistics for a particular player pool across a sport. 
#' Acceptable values include: All, Qualified, Rookies, or Qualified_rookies
#' @param game_type Game type to return information for a particular statistic in a particular game type.
#' @param team_id Team ID to return information and ranking for a particular statistic for a particular team.
#' @param position Position to return statistics for a given position. Default to "Qualified" player pool
#' Acceptable values include: 
#'    - P
#'    - C
#'    - 1B
#'    - 2B
#'    - 3B
#'    - SS
#'    - LF
#'    - CF
#'    - RF
#'    - DH
#'    - PH
#'    - PR
#'    - BR
#'    - OF
#'    - IF
#'    - SP
#'    - RP
#'    - CP
#'    - UT
#'    - UI
#'    - UO
#'    - RHP
#'    - LHP
#'    - RHS
#'    - LHS
#'    - LHR
#'    - RHR
#'    - B
#'    - X
#' @param stat_group Stat group to return information and ranking for a particular statistic in a particular group.
#' @param season Year to return information and ranking for a particular statistic in a given year. 
#' @param league_id League ID to return statistics for a given league. Default to "Qualified" player pool.
#' @param sport_ids The sport_id(s) to return information and ranking information for.
#' @param sort_stat Sort return based on stat.
#' @param order Order return based on either desc or asc.
#' @param limit A limit to limit return to a particular number of records.
#' @param offset An offset to returns i+1 as the first record in the set of players.
#' 
#' @return Returns a tibble with the following columns
#'   |col_name                |types     |
#'   |:-----------------------|:---------|
#'   |total_splits            |integer   |
#'   |season                  |character |
#'   |num_teams               |integer   |
#'   |rank                    |integer   |
#'   |games_played            |integer   |
#'   |ground_outs             |integer   |
#'   |air_outs                |integer   |
#'   |runs                    |integer   |
#'   |doubles                 |integer   |
#'   |triples                 |integer   |
#'   |home_runs               |integer   |
#'   |strike_outs             |integer   |
#'   |base_on_balls           |integer   |
#'   |intentional_walks       |integer   |
#'   |hits                    |integer   |
#'   |hit_by_pitch            |integer   |
#'   |avg                     |character |
#'   |at_bats                 |integer   |
#'   |obp                     |character |
#'   |slg                     |character |
#'   |ops                     |character |
#'   |caught_stealing         |integer   |
#'   |stolen_bases            |integer   |
#'   |stolen_base_percentage  |character |
#'   |ground_into_double_play |integer   |
#'   |number_of_pitches       |integer   |
#'   |plate_appearances       |integer   |
#'   |total_bases             |integer   |
#'   |rbi                     |integer   |
#'   |left_on_base            |integer   |
#'   |sac_bunts               |integer   |
#'   |sac_flies               |integer   |
#'   |babip                   |character |
#'   |ground_outs_to_airouts  |character |
#'   |catchers_interference   |integer   |
#'   |at_bats_per_home_run    |character |
#'   |team_id                 |integer   |
#'   |team_name               |character |
#'   |team_link               |character |
#'   |player_id               |integer   |
#'   |player_full_name        |character |
#'   |player_link             |character |
#'   |player_first_name       |character |
#'   |player_last_name        |character |
#'   |league_id               |integer   |
#'   |league_name             |character |
#'   |league_link             |character |
#'   |sport_id                |integer   |
#'   |sport_link              |character |
#'   |sport_abbreviation      |character |
#'   |position_code           |character |
#'   |position_name           |character |
#'   |position_type           |character |
#'   |position_abbreviation   |character |
#'   |splits_tied_with_offset |list      |
#'   |splits_tied_with_limit  |list      |
#'   |player_pool             |character |
#'   |type_display_name       |character |
#'   |group_display_name      |character |
#' @export
#' @examples \donttest{
#'   try(mlb_stats(stat_type = 'season', stat_group = 'hitting', season = 2021))
#' }
mlb_stats <- function(stat_type = NULL,
                      player_pool=NULL,
                      game_type = NULL,
                      team_id = NULL,
                      position = NULL,
                      stat_group = NULL,
                      season = NULL,
                      league_id = NULL,
                      sport_ids = NULL,
                      sort_stat = NULL,
                      order = NULL,
                      limit = 1000,
                      offset = NULL){
  
  sport_ids <- paste(sport_ids, collapse = ',')
  mlb_endpoint <- mlb_stats_endpoint("v1/stats")
  query_params <- list(
    stats = stat_type,
    playerPool = player_pool,
    gameType = game_type,
    teamId = team_id,
    position = position,
    group = stat_group,
    season = season,
    leagueId = league_id,
    sportIds = sport_ids,
    sortStat = sort_stat,
    order = order,
    limit = limit,
    offset = offset
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      stats_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)  
      stats_leaders$season <- NULL
      stats <- stats_leaders %>% 
        tidyr::unnest("splits") %>% 
        janitor::clean_names()  %>% 
        as.data.frame() %>% 
        dplyr::select(-"exemptions") %>%
        make_baseballr_data("MLB Stats data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  colnames(stats)<-gsub("stat_", "", colnames(stats))
  return(stats)
}


#' @rdname mlb_stats_endpoints
#' @name mlb_stats_endpoints
#' @aliases mlb_stats_endpoints stats
#' @title
#' **MLB Stats Endpoint Overview**
#' @description
#'
#' * `mlb_stats()`: MLB Stats.
#' * `mlb_stats_leaders()`: MLB Stats Leaders.
#' * `mlb_stats_streaks()`: MLB Stats Streaks.
#' * `mlb_stat_groups()`: MLB Stat Groups.
#' * `mlb_stat_types()`: MLB Stat Types.
#' * `mlb_baseball_stats()`: MLB Baseball Stats.
#'
#' @details
#' ## **MLB Stats**
#'
#' These functions retrieve MLB statistics, leaders, streaks, stat groups, stat types, and baseball stat metadata from the MLB Stats API.
#'
#' @family MLB Stats
NULL

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
#'
#'   |col_name                   |types     |description                                      |
#'   |:--------------------------|:---------|:------------------------------------------------|
#'   |total_splits               |integer   |Total number of splits in the response.          |
#'   |season                     |character |Season year for the statistic.                   |
#'   |num_teams                  |integer   |Number of teams the player appeared for.         |
#'   |rank                       |integer   |Rank of the player for the sorted statistic.     |
#'   |age                        |integer   |Player age during the season.                    |
#'   |games_played               |integer   |Games played.                                    |
#'   |ground_outs                |integer   |Ground outs.                                     |
#'   |air_outs                   |integer   |Air outs (fly outs).                             |
#'   |runs                       |integer   |Runs scored.                                     |
#'   |doubles                    |integer   |Doubles.                                         |
#'   |triples                    |integer   |Triples.                                         |
#'   |home_runs                  |integer   |Home runs.                                       |
#'   |strike_outs                |integer   |Strikeouts.                                      |
#'   |base_on_balls              |integer   |Walks (bases on balls).                          |
#'   |intentional_walks          |integer   |Intentional walks.                               |
#'   |hits                       |integer   |Hits.                                            |
#'   |hit_by_pitch               |integer   |Times hit by pitch.                              |
#'   |avg                        |character |Batting average.                                 |
#'   |at_bats                    |integer   |At bats.                                         |
#'   |obp                        |character |On-base percentage.                              |
#'   |slg                        |character |Slugging percentage.                             |
#'   |ops                        |character |On-base plus slugging.                           |
#'   |caught_stealing            |integer   |Times caught stealing.                           |
#'   |stolen_bases               |integer   |Stolen bases.                                    |
#'   |stolen_base_percentage     |character |Stolen base success percentage.                  |
#'   |caught_stealing_percentage |character |Caught stealing percentage.                      |
#'   |ground_into_double_play    |integer   |Grounded into double plays.                      |
#'   |number_of_pitches          |integer   |Total pitches seen.                              |
#'   |plate_appearances          |integer   |Plate appearances.                               |
#'   |total_bases                |integer   |Total bases.                                     |
#'   |rbi                        |integer   |Runs batted in.                                  |
#'   |left_on_base               |integer   |Runners left on base.                            |
#'   |sac_bunts                  |integer   |Sacrifice bunts.                                 |
#'   |sac_flies                  |integer   |Sacrifice flies.                                 |
#'   |babip                      |character |Batting average on balls in play.                |
#'   |ground_outs_to_airouts     |character |Ratio of ground outs to air outs.                |
#'   |catchers_interference      |integer   |Times reached on catcher's interference.         |
#'   |at_bats_per_home_run       |character |At bats per home run.                            |
#'   |team_id                    |integer   |Team MLBAM ID.                                   |
#'   |team_name                  |character |Team name.                                       |
#'   |team_link                  |character |API link to the team.                            |
#'   |player_id                  |integer   |Player MLBAM ID.                                 |
#'   |player_full_name           |character |Player full name.                                |
#'   |player_link                |character |API link to the player.                          |
#'   |player_first_name          |character |Player first name.                               |
#'   |player_last_name           |character |Player last name.                                |
#'   |league_id                  |integer   |League MLBAM ID.                                 |
#'   |league_name                |character |League name.                                     |
#'   |league_link                |character |API link to the league.                          |
#'   |sport_id                   |integer   |Sport MLBAM ID.                                  |
#'   |sport_link                 |character |API link to the sport.                           |
#'   |sport_abbreviation         |character |Sport abbreviation (e.g., MLB).                  |
#'   |position_code              |character |Primary position code.                           |
#'   |position_name              |character |Primary position name.                           |
#'   |position_type              |character |Primary position type.                           |
#'   |position_abbreviation      |character |Primary position abbreviation.                   |
#'   |splits_tied_with_offset    |list      |Players tied at the offset boundary.             |
#'   |splits_tied_with_limit     |list      |Players tied at the limit boundary.              |
#'   |player_pool                |character |Player pool used (e.g., QUALIFIED).              |
#'   |type_display_name          |character |Stat type display name.                          |
#'   |group_display_name         |character |Stat group display name.                         |
#'
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

  stats <- NULL

  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      stats_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)  
      stats_leaders$season <- NULL
      stats <- stats_leaders |> 
        tidyr::unnest("splits") |> 
        janitor::clean_names()  |> 
        as.data.frame() |> 
        dplyr::select(-dplyr::any_of("exemptions")) |>
        make_baseballr_data("MLB Stats data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  if (!is.null(stats)) {
    colnames(stats) <- gsub("stat_", "", colnames(stats))
  }
  return(stats)
}


#' @title **MLB Team Individual Stats**
#' @param stat_type Stat type to return statistics for.
#' @param game_type Game type to return information for a particular statistic in a particular game type.
#' @param team_id Team ID to return information and ranking for a particular statistic for a particular team.
#' @param stat_group Stat group to return information and ranking for a particular statistic in a particular group.
#' @param season Year to return information and ranking for a particular statistic in a given year. 
#' @param sport_ids The sport_id(s) to return information and ranking information for.
#' 
#' @return Returns a tibble with the following columns
#'    |col_name                   |types     |description                              |
#'    |:--------------------------|:---------|:----------------------------------------|
#'    |season                     |character |Season year for the statistic.           |
#'    |games_played               |integer   |Games played.                            |
#'    |ground_outs                |integer   |Ground outs.                             |
#'    |air_outs                   |integer   |Air outs (fly outs).                     |
#'    |runs                       |integer   |Runs scored.                             |
#'    |doubles                    |integer   |Doubles.                                 |
#'    |triples                    |integer   |Triples.                                 |
#'    |home_runs                  |integer   |Home runs.                               |
#'    |strike_outs                |integer   |Strikeouts.                              |
#'    |base_on_balls              |integer   |Walks (bases on balls).                  |
#'    |intentional_walks          |integer   |Intentional walks.                       |
#'    |hits                       |integer   |Hits.                                    |
#'    |hit_by_pitch               |integer   |Times hit by pitch.                      |
#'    |avg                        |character |Batting average.                         |
#'    |at_bats                    |integer   |At bats.                                 |
#'    |obp                        |character |On-base percentage.                      |
#'    |slg                        |character |Slugging percentage.                     |
#'    |ops                        |character |On-base plus slugging.                   |
#'    |caught_stealing            |integer   |Times caught stealing.                   |
#'    |stolen_bases               |integer   |Stolen bases.                            |
#'    |stolen_base_percentage     |character |Stolen base success percentage.          |
#'    |caught_stealing_percentage |character |Caught stealing percentage.              |
#'    |ground_into_double_play    |integer   |Grounded into double plays.              |
#'    |number_of_pitches          |integer   |Total pitches seen.                      |
#'    |plate_appearances          |integer   |Plate appearances.                       |
#'    |total_bases                |integer   |Total bases.                             |
#'    |rbi                        |integer   |Runs batted in.                          |
#'    |left_on_base               |integer   |Runners left on base.                    |
#'    |sac_bunts                  |integer   |Sacrifice bunts.                         |
#'    |sac_flies                  |integer   |Sacrifice flies.                         |
#'    |babip                      |character |Batting average on balls in play.        |
#'    |ground_outs_to_airouts     |character |Ratio of ground outs to air outs.        |
#'    |catchers_interference      |integer   |Times reached on catcher's interference. |
#'    |at_bats_per_home_run       |character |At bats per home run.                    |
#'    |team_id                    |integer   |Team MLBAM ID.                           |
#'    |team_name                  |character |Team name.                               |
#'    |team_link                  |character |API link to the team.                    |
#'    |type_display_name          |character |Stat type display name.                  |
#'    |group_display_name         |character |Stat group display name.                 |
#' @export
#' @examples \donttest{
#'   try(mlb_team_stats(team_id = 137, stat_type = 'season', stat_group = 'hitting', season = 2021))
#' }
mlb_team_stats <- function(team_id = NULL,
                           stat_type = NULL,
                           game_type = NULL,
                           stat_group = NULL,
                           season = NULL,
                           sport_ids = NULL){
  
  sport_ids <- paste(sport_ids, collapse = ',')
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}/stats"))
  query_params <- list(
    stats = stat_type,
    gameType = game_type,
    group = stat_group,
    season = season,
    sportIds = sport_ids
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)

  stats <- NULL

  tryCatch(
    expr={
      resp <- mlb_endpoint |> 
        mlb_api_call()
      stats_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)  
      stats_leaders$season <- NULL
      stats <- stats_leaders |> 
        tidyr::unnest("splits") |> 
        janitor::clean_names()  |> 
        as.data.frame() |> 
        dplyr::select(-dplyr::any_of("exemptions")) |>
        make_baseballr_data("MLB Team Stats data from MLB.com",Sys.time())
      
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


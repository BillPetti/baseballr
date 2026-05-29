#' @title **MLB Teams Stats Leaders**
#' @param leader_categories League leader category to return information and ranking for a particular statistic.
#' @param leader_game_types Game type to return information and ranking for a particular statistic in a particular game type.
#' @param sit_codes Situation code to return information and ranking for a particular statistic in a particular game type.
#' @param stat_group Stat group to return information and ranking for a particular statistic in a particular group.
#' @param season Year to return information and ranking for a particular statistic in a given year. 
#' @param league_id League ID to return statistics for a given league. Default to "Qualified" player pool.
#' @param sport_id The sport_id to return information and ranking information for.
#' @param start_date Start date to return information and ranking for a particular statistic for a particular date range. Format: MM/DD/YYYY
#'  *start_date must be coupled with end_date and byDateRange stat_type*
#' @param end_date End date to return information and ranking for a particular statistic for a particular date range. Format: MM/DD/YYYY
#'   *end_date must be coupled with start_date and byDateRange stat_type*
#' @param stat_type The stat_type to return information and ranking for a particular statistic for a particular stat type.
#' @param limit A limit to limit return to a particular number of records.
#' @return Returns a tibble with the following columns
#'
#'   |col_name              |types     |description                                  |
#'   |:---------------------|:---------|:--------------------------------------------|
#'   |leader_category       |character |Team leader category (e.g., homeRuns).       |
#'   |rank                  |integer   |Rank within the team leaderboard.            |
#'   |value                 |character |Statistic value for the team.                |
#'   |season                |character |Season year.                                 |
#'   |team_id               |integer   |Team MLBAM ID.                               |
#'   |team_name             |character |Team name.                                   |
#'   |team_link             |character |API link to the team.                        |
#'   |stat_group            |character |Stat group (e.g., hitting).                  |
#'   |total_splits          |integer   |Total number of splits in the leaderboard.   |
#'   |game_type_id          |character |Game type code (e.g., R for regular season). |
#'   |game_type_description |character |Game type description.                       |
#'
#' @export
#' @examples \donttest{
#'  try(mlb_teams_stats_leaders(leader_categories='homeRuns',sport_id=1, season = 2021))
#' }
mlb_teams_stats_leaders <- function(leader_categories = NULL,
                                    leader_game_types = NULL,
                                    sit_codes = NULL,
                                    stat_group = NULL,
                                    season = NULL,
                                    league_id = NULL,
                                    sport_id = NULL,
                                    start_date = NULL,
                                    end_date = NULL,
                                    stat_type = NULL,
                                    limit = 1000){
  
  sport_id <- paste(sport_id, collapse = ',')
  mlb_endpoint <- mlb_stats_endpoint("v1/teams/stats/leaders")
  query_params <- list(
    leaderCategories = leader_categories,
    leaderGameTypes = leader_game_types,
    sitCodes = sit_codes,
    statGroup = stat_group,
    season = season,
    leagueId = league_id,
    sportId = sport_id,
    startDate = start_date,
    endDate = end_date,
    statType = stat_type,
    limit = limit
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  stats_leaders <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      stats_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['leagueLeaders']]), flatten = TRUE)  
      stats_leaders$season <- NULL
      stats_leaders <- stats_leaders |> 
        tidyr::unnest("leaders") |> 
        janitor::clean_names()  |> 
        as.data.frame()  |>
        make_baseballr_data("MLB Teams Stats Leaders data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(stats_leaders)
}


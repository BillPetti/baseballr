#' @title **MLB Team Leaders**
#' @param team_id Team ID to return team leader information for.
#' @param leader_categories Team leader category to return information and ranking for a particular statistic.
#' @param leader_game_types Game type to return information and ranking for a particular statistic in a particular game type.
#' @param season Season to return team leader information for.
#' @param limit A limit to limit return to a particular number of records.
#' @return Returns a tibble with the following columns
#'
#'   |col_name              |types     |description                                  |
#'   |:---------------------|:---------|:--------------------------------------------|
#'   |leader_category       |character |Team leader category (e.g., homeRuns).       |
#'   |rank                  |integer   |Rank within the team leaderboard.            |
#'   |value                 |character |Statistic value for the leader.              |
#'   |season                |character |Season year.                                 |
#'   |team_id               |integer   |Team MLBAM ID.                               |
#'   |team_name             |character |Team name.                                   |
#'   |team_link             |character |API link to the team.                        |
#'   |league_id             |integer   |League MLBAM ID.                             |
#'   |league_name           |character |League name.                                 |
#'   |league_link           |character |API link to the league.                      |
#'   |person_id             |integer   |Player MLBAM ID.                             |
#'   |person_full_name      |character |Player full name.                            |
#'   |person_link           |character |API link to the player.                      |
#'   |person_first_name     |character |Player first name.                           |
#'   |person_last_name      |character |Player last name.                            |
#'   |sport_id              |integer   |Sport MLBAM ID.                              |
#'   |sport_link            |character |API link to the sport.                       |
#'   |sport_abbreviation    |character |Sport abbreviation (e.g., MLB).              |
#'   |stat_group            |character |Stat group (e.g., hitting).                  |
#'   |total_splits          |integer   |Total number of splits in the leaderboard.   |
#'   |game_type_id          |character |Game type code (e.g., R for regular season). |
#'   |game_type_description |character |Game type description.                       |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_team_leaders(team_id = 137, leader_categories = "homeRuns", season = 2021))
#' }
mlb_team_leaders <- function(team_id = NULL,
                             leader_categories = NULL,
                             leader_game_types = NULL,
                             season = NULL,
                             limit = 1000){
  
  
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}/leaders"))
  query_params <- list(
    leaderCategories = leader_categories,
    leaderGameTypes = leader_game_types,
    season = season,
    limit = limit
  )
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  team_leaders <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      team_leaders <- jsonlite::fromJSON(jsonlite::toJSON(resp[['teamLeaders']]), flatten = TRUE)  
      team_leaders$season <- NULL
      team_leaders$team.id <- NULL
      team_leaders$team.name <- NULL
      team_leaders$team.link <- NULL
      team_leaders <- team_leaders |> 
        tidyr::unnest("leaders") |> 
        janitor::clean_names() |> 
        as.data.frame() |>
        make_baseballr_data("MLB Team Leaders data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  
  
  return(team_leaders)
}


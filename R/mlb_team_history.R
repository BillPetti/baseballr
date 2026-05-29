#' @title **MLB Teams History** 
#' @param team_ids The team_id(s) to return historical data for.
#' @param start_season The start_season to return historical data for from the given year to present.
#' @param end_season The end_season to return historical data for from the the creation to the given year.
#' @return Returns a tibble with the following columns
#'
#'   |col_name           |types     |description                                |
#'   |:------------------|:---------|:------------------------------------------|
#'   |all_star_status    |character |All-star status flag.                      |
#'   |team_id            |integer   |Team MLBAM ID.                             |
#'   |team_full_name     |character |Full team name.                            |
#'   |link               |character |API link to the team.                      |
#'   |season             |integer   |Season year for the historical record.     |
#'   |team_code          |character |Internal team code.                        |
#'   |file_code          |character |File code abbreviation.                    |
#'   |team_abbreviation  |character |Team abbreviation.                         |
#'   |team_name          |character |Short team name.                           |
#'   |location_name      |character |Team location (city).                      |
#'   |first_year_of_play |character |First year the franchise played.           |
#'   |short_name         |character |Short display name.                        |
#'   |franchise_name     |character |Franchise name.                            |
#'   |club_name          |character |Club name.                                 |
#'   |active             |logical   |Whether the team is active.                |
#'   |venue_id           |integer   |Home venue MLBAM ID for that season.       |
#'   |venue_name         |character |Home venue name for that season.           |
#'   |venue_link         |character |API link to the venue.                     |
#'   |spring_venue_id    |integer   |Spring training venue MLBAM ID.            |
#'   |spring_venue_link  |character |API link to the spring venue.              |
#'   |league_id          |integer   |League MLBAM ID.                           |
#'   |league_name        |character |League name.                               |
#'   |league_link        |character |API link to the league.                    |
#'   |sport_id           |integer   |Sport MLBAM ID.                            |
#'   |sport_link         |character |API link to the sport.                     |
#'   |sport_name         |character |Sport name (e.g., Major League Baseball).  |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_team_history(team_ids = 147))
#' }
mlb_team_history <- function(team_ids = NULL, 
                             start_season = NULL, 
                             end_season = NULL){
  team_ids <- paste(team_ids, collapse = ',')
  mlb_endpoint <- mlb_stats_endpoint("v1/teams/history")
  query_params <- list(
    teamIds = team_ids, 
    startSeason = start_season, 
    endSeason = end_season
  )
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  teams <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      teams <- jsonlite::fromJSON(jsonlite::toJSON(resp$teams),flatten = TRUE) |> 
        janitor::clean_names() |> 
        dplyr::rename(
          "team_id" = "id",
          "team_full_name" = "name",
          "team_abbreviation" = "abbreviation") |>
        make_baseballr_data("MLB Team History data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(teams)
}

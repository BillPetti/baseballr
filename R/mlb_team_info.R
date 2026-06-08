#' @title **MLB Team Info** 
#' @param team_id The team_id to return team data for.
#' @param season The season to return team data for the given year.
#' @param sport_id The sport_id to return a directory of team data for a particular club in a sport.
#' @return Returns a tibble with the following columns
#'
#'   |col_name                   |types     |description                                |
#'   |:--------------------------|:---------|:------------------------------------------|
#'   |all_star_status            |character |All-star status flag.                      |
#'   |team_id                    |integer   |Team MLBAM ID.                             |
#'   |team_full_name             |character |Full team name.                            |
#'   |link                       |character |API link to the team.                      |
#'   |season                     |integer   |Season year.                               |
#'   |team_code                  |character |Internal team code.                        |
#'   |file_code                  |character |File code abbreviation.                    |
#'   |team_abbreviation          |character |Team abbreviation.                         |
#'   |team_name                  |character |Short team name.                           |
#'   |location_name              |character |Team location (city).                      |
#'   |first_year_of_play         |character |First year the franchise played.           |
#'   |short_name                 |character |Short display name.                        |
#'   |franchise_name             |character |Franchise name.                            |
#'   |club_name                  |character |Club name.                                 |
#'   |active                     |logical   |Whether the team is active.                |
#'   |spring_league_id           |integer   |Spring league MLBAM ID.                    |
#'   |spring_league_name         |character |Spring league name.                        |
#'   |spring_league_link         |character |API link to the spring league.             |
#'   |spring_league_abbreviation |character |Spring league abbreviation.                |
#'   |venue_id                   |integer   |Home venue MLBAM ID.                       |
#'   |venue_name                 |character |Home venue name.                           |
#'   |venue_link                 |character |API link to the venue.                     |
#'   |spring_venue_id            |integer   |Spring training venue MLBAM ID.            |
#'   |spring_venue_link          |character |API link to the spring venue.              |
#'   |league_id                  |integer   |League MLBAM ID.                           |
#'   |league_name                |character |League name.                               |
#'   |league_link                |character |API link to the league.                    |
#'   |division_id                |integer   |Division MLBAM ID.                         |
#'   |division_name              |character |Division name.                             |
#'   |division_link              |character |API link to the division.                  |
#'   |sport_id                   |integer   |Sport MLBAM ID.                            |
#'   |sport_link                 |character |API link to the sport.                     |
#'   |sport_name                 |character |Sport name (e.g., Major League Baseball).  |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_team_info(team_id = 147))
#' }
mlb_team_info <- function(team_id = NULL, 
                          season = NULL, 
                          sport_id = NULL){
  mlb_endpoint <- mlb_stats_endpoint(glue::glue("v1/teams/{team_id}"))
  query_params <- list(
    season = season, 
    sportId = sport_id
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
        make_baseballr_data("MLB Team Info data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(teams)
}

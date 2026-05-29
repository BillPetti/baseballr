#' @title **MLB Divisions** 
#' @param division_id Return division(s) data for a specific division
#' @param league_id Return division(s) data for all divisions in a specific league
#' @param sport_id Return division(s) for all divisions in a specific sport.
#' @return Returns a tibble with the following columns
#'
#'  |col_name              |types     |description                                     |
#'  |:---------------------|:---------|:-----------------------------------------------|
#'  |division_id           |integer   |MLB division ID.                                |
#'  |division_name         |character |Division name.                                  |
#'  |season                |character |Season (YYYY).                                  |
#'  |division_name_short   |character |Short division name.                            |
#'  |division_link         |character |MLB Stats API relative division link.           |
#'  |division_abbreviation |character |Division abbreviation.                          |
#'  |has_wildcard          |logical   |Whether the division has a wild card.           |
#'  |sort_order            |integer   |Display sort order for the division.            |
#'  |num_playoff_teams     |integer   |Number of playoff teams from the division.      |
#'  |active                |logical   |Whether the division is currently active.       |
#'  |league_id             |integer   |MLB league ID.                                  |
#'  |league_link           |character |MLB Stats API relative league link.             |
#'  |sport_id              |integer   |MLB sport ID.                                   |
#'  |sport_link            |character |MLB Stats API relative sport link.              |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_divisions(sport_id = 1))
#' }
mlb_divisions <- function(division_id = NULL,
                          league_id = NULL,
                          sport_id = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/divisions")
  query_params <- list(
    divisionId = division_id,
    leagueId = league_id,
    sportId = sport_id
  )
  
  mlb_endpoint <- httr2::url_modify_query(mlb_endpoint, !!!query_params)
  
  divisions <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      divisions <- jsonlite::fromJSON(jsonlite::toJSON(resp$divisions), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        as.data.frame() |> 
        dplyr::rename(
          "division_id" = "id",
          "division_name" = "name",
          "division_name_short" = "name_short",
          "division_link" = "link",
          "division_abbreviation" = "abbreviation") |>
        make_baseballr_data("MLB Divisions data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(divisions)
}


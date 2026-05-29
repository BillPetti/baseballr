#' @title **View all PCL conferences** 
#' @param season Year to return to return conference information for.
#' @param conference_id Conference ID to return information for.
#' @return Returns a tibble with the following columns
#'
#'   |col_name                |types     |description                                  |
#'   |:-----------------------|:---------|:--------------------------------------------|
#'   |conference_id           |integer   |MLB conference ID.                           |
#'   |conference_name         |character |Conference name.                             |
#'   |link                    |character |MLB Stats API relative conference link.      |
#'   |conference_abbreviation |character |Conference abbreviation.                     |
#'   |has_wildcard            |logical   |Whether the conference has a wild card.      |
#'   |conference_name_short   |character |Short conference name.                       |
#'   |league_id               |integer   |MLB league ID.                               |
#'   |league_link             |character |MLB Stats API relative league link.          |
#'   |sport_id                |integer   |MLB sport ID.                                |
#'   |sport_link              |character |MLB Stats API relative sport link.           |
#'
#' @export
#' @examples \donttest{
#'   try(mlb_conferences())
#'   try(mlb_conferences(conference_id =  301, season = 2020))
#' }
mlb_conferences <- function(conference_id = NULL,
                            season = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/conferences")
  query_params <- list(
    conferenceId = conference_id,
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  conferences <- NULL
  tryCatch(
    expr = {
      resp <- mlb_endpoint |> 
        mlb_api_call()
      conferences <- jsonlite::fromJSON(jsonlite::toJSON(resp$conferences), flatten = TRUE)  |> 
        janitor::clean_names() |> 
        dplyr::rename(
          "conference_id" = "id",
          "conference_name" = "name",
          "conference_abbreviation" = "abbreviation",
          "conference_name_short" = "name_short") |>
        make_baseballr_data("MLB Conferences data from MLB.com",Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments provided")
    },
    finally = {
    }
  )
  return(conferences)
}


#' @title **View all PCL conferences** 
#' @param season Year to return to return conference information for.
#' @param conference_id Conference ID to return information for.
#' @return Returns a data frame with the following columns
#'   |col_name                |types     |
#'   |:-----------------------|:---------|
#'   |conference_id           |integer   |
#'   |conference_name         |character |
#'   |link                    |character |
#'   |conference_abbreviation |character |
#'   |has_wildcard            |logical   |
#'   |name_short              |character |
#'   |league_id               |integer   |
#'   |league_link             |character |
#'   |sport_id                |integer   |
#'   |sport_link              |character |
#' @export
#' @examples \donttest{
#'   mlb_conferences()
#'   mlb_conferences(conference_id =  301, season = 2021)
#' }
mlb_conferences <- function(
  conference_id = NULL,
  season = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/conferences")
  query_params <- list(
    conferenceId = conference_id,
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  conferences <- jsonlite::fromJSON(jsonlite::toJSON(resp$conferences), flatten = TRUE)  %>% 
    janitor::clean_names() %>% 
    dplyr::rename(
      conference_id = .data$id,
      conference_name = .data$name,
      conference_abbreviation = .data$abbreviation,
      conference_name_short = .data$name_short)
  return(conferences)
}


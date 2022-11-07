#' @title **View all PCL conferences** 
#' @param season Year to return to return conference information for.
#' @param conference_id Conference ID to return information for.
#' @return Returns a tibble with the following columns
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
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      conferences <- jsonlite::fromJSON(jsonlite::toJSON(resp$conferences), flatten = TRUE)  %>% 
        janitor::clean_names() %>% 
        dplyr::rename(
          conference_id = .data$id,
          conference_name = .data$name,
          conference_abbreviation = .data$abbreviation,
          conference_name_short = .data$name_short) %>%
        make_baseballr_data("MLB Conferences data from MLB.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(conferences)
}


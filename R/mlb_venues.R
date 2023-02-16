#' @title **Find MLB Venues**
#' @param venue_ids Venue directorial information based venue_id.
#' @param sport_ids The sport_id(s) for which to return venue directorial information.
#' @param season Year for which to return venue directorial information for a given season.
#' @return Returns a tibble with the following columns:
#'  |col_name   |types     |
#'  |:----------|:---------|
#'  |venue_id   |integer   |
#'  |venue_name |character |
#'  |venue_link |character |
#'  |active     |logical   |
#'  |season     |logical   |
#' @importFrom jsonlite fromJSON 
#' @export
#' @examples \donttest{
#'   try(mlb_venues())
#'   try(mlb_venues(venue_ids = 4781))
#'   try(mlb_venues(sport_ids = 1))
#' }
mlb_venues <- function(venue_ids = NULL, sport_ids = NULL, season = NULL){
  mlb_endpoint <- mlb_stats_endpoint("v1/venues")
  query_params <- list(
    venueIds = venue_ids, 
    sportIds = sport_ids, 
    season = season
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr = {
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      
      venues <- resp$venues
      colnames(venues) <- c("venue_id", "venue_name", "venue_link","active", "season")
      venues <- venues %>%
        make_baseballr_data("MLB Venues data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    finally = {
    }
  )
  return(venues)
}

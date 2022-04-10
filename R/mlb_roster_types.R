#' @title **MLB Roster Types** 
#' @return Returns a data frame with the following columns
#'  |col_name                |types     |
#'  |:-----------------------|:---------|
#'  |roster_type_description |character |
#'  |roster_type_lookup_name |character |
#'  |roster_type_parameter   |character |
#' @export
#' @examples \donttest{
#'   try(mlb_roster_types())
#' }
mlb_roster_types <- function(){
  mlb_endpoint <- mlb_stats_endpoint("v1/rosterTypes")
  resp <- mlb_endpoint %>% 
    mlb_api_call()
  roster_types <- resp %>% 
    janitor::clean_names() %>% 
    dplyr::rename(
      roster_type_description = .data$description,
      roster_type_lookup_name = .data$lookup_name,
      roster_type_parameter = .data$parameter
    )
  return(roster_types)
}
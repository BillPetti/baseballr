#' @title Lookup NCAA School IDs (Division I, II, and III)
#' @description This function allows the user to lookup the school_id needed for the ncaa_scrape function.
#' @param school_name A string that will be searched for in the names of the schools.
#' @export
#' @examples \donttest{
#'   school_id_lu("Van")
#' }

school_id_lu <- function(school_name = NULL) {
  x <- baseballr::master_ncaa_team_lu %>%
    dplyr::filter(grepl(school_name, .data$school))
  return(x)
}
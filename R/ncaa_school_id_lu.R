#' @rdname ncaa_school_id_lu
#' @title **Lookup NCAA baseball school IDs (Division I, II, and III)**
#' @description This function allows the user to lookup the school_id needed for the ncaa_scrape function.
#' @param school_name A string that will be searched for in the names of the schools.
#' @export
#' @examples \donttest{
#'   ncaa_school_id_lu("Van")
#' }

ncaa_school_id_lu <- function(school_name = NULL) {
  x <- baseballr::ncaa_team_lu %>%
    dplyr::filter(grepl(school_name, .data$school))
  return(x)
}

#' @rdname ncaa_school_id_lu
#' @export

school_id_lu <-  ncaa_school_id_lu
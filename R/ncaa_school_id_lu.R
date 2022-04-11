#' @rdname ncaa_school_id_lu
#' @title **Lookup NCAA baseball school IDs (Division I, II, and III)**
#' @description This function allows the user to look up the `school_id` needed for the ```ncaa_scrape()``` function.
#' @param school_name A string that will be searched for in the names of the schools.
#' @return Returns a data frame with school identification data: school, conference, school_id, year, division, conference_id
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |school        |character |
#'  |conference    |character |
#'  |school_id     |numeric   |
#'  |year          |numeric   |
#'  |division      |numeric   |
#'  |conference_id |numeric   |
#' @export
#' @examples \donttest{
#'   try(ncaa_school_id_lu("Van"))
#' }

ncaa_school_id_lu <- function(school_name = NULL) {
  x <- baseballr::ncaa_team_lu %>%
    dplyr::filter(grepl(school_name, .data$school))
  return(x)
}

#' @rdname school_id_lu
#' @title **(legacy) Lookup NCAA baseball school IDs (Division I, II, and III)**
#' @inheritParams ncaa_school_id_lu
#' @return Returns a data frame with school identification data: school, conference, school_id, year, division, conference_id
#' @keywords legacy
#' @export
school_id_lu <-  ncaa_school_id_lu
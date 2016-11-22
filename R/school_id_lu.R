#' Lookup NCAA School IDs (Division I, II, and III)
#'
#' This function allows the user to lookup the school_id needed for the ncaa_scrape function.
#'
#' @param school_name A string that will be searched for in the names of the schools.
#' @keywords baseball, NCAA, college
#' @importFrom dplyr "%>%" dplyr filter
#' @export school_id_lu
#' @examples
#' \dontrun{school_id_lu("Van")}

school_id_lu <- function(school_name = NULL) {
  x <- master_ncaa_team_lu %>%
    filter(grepl(school_name, school))
  x
}

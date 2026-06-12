#' @rdname ncaa_school_id_lu
#' @title **Lookup NCAA baseball school IDs (Division I, II, and III)**
#' @description This function allows the user to look up the `team_id` needed 
#' for the ```ncaa_team_player_stats()``` function.
#' @param team_name A string that will be searched for in the names of the teams.
#' @return Returns a tibble with school identification data: team_id, team_name, team_url,
#'  conference, conference_id, division, year, and season_id
#' 
#'  |col_name      |types     |description                            |
#'  |:-------------|:---------|:--------------------------------------|
#'  |team_id       |numeric   |Team NCAA id.                          |
#'  |team_name     |character |Team name.                             |
#'  |team_url      |character |Relative stats.ncaa.org team url.      |
#'  |conference_id |numeric   |Conference identifier.                 |
#'  |conference    |character |Conference name.                       |
#'  |division      |numeric   |NCAA division (1, 2, 3).               |
#'  |year          |numeric   |Season (4-digit year).                 |
#'  |season_id     |numeric   |stats.ncaa.org season identifier.      |
#'  
#' @details
#' Example usage (resolved from the bundled NCAA teams table):
#'
#' ```r
#' ncaa_school_id_lu("Van")
#' ```
#' @export

ncaa_school_id_lu <- function(team_name = NULL) {
  if (is.null(team_name)) {
    cli::cli_abort("Enter valid team_name")
  }
  x <- load_ncaa_baseball_teams() %>%
    dplyr::filter(grepl({{team_name}}, .data$team_name))
  return(x)
}

#' @rdname school_id_lu
#' @title **(legacy) Lookup NCAA baseball school IDs (Division I, II, and III)**
#' @inheritParams ncaa_school_id_lu
#' @inherit ncaa_school_id_lu return
#' @keywords legacy
#' @export
school_id_lu <-  ncaa_school_id_lu

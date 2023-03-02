#' @rdname ncaa_school_id_lu
#' @title **Lookup NCAA baseball school IDs (Division I, II, and III)**
#' @description This function allows the user to look up the `team_id` needed 
#' for the ```ncaa_team_player_stats()``` function.
#' @param team_name A string that will be searched for in the names of the teams.
#' @return Returns a tibble with school identification data: team_id, team_name, team_url,
#'  conference, conference_id, division, year, and season_id
#' 
#'  |col_name      |types     |
#'  |:-------------|:---------|
#'  |team_id       |numeric   |
#'  |team_name     |character |
#'  |team_url      |character |
#'  |conference_id |numeric   |
#'  |conference    |character |
#'  |division      |numeric   |
#'  |year          |numeric   |
#'  |season_id     |numeric   |
#'  
#' @export
#' @examples \donttest{
#'   try(ncaa_school_id_lu("Van"))
#' }

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
#' @return Returns a tibble with school identification data: team_id, team_name, team_url,
#'  conference, conference_id, division, year, and season_id
#' @keywords legacy
#' @export
school_id_lu <-  ncaa_school_id_lu

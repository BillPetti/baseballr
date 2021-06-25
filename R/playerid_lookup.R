#' Look up Baseball Player IDs
#'
#' This function allows you to query the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' @param last_name A text string used to return results for players with that string in their last name.
#' @param first_name A text string used to return results for players with that string in their first name.
#' @keywords MLB, sabermetrics
#' @importFrom vroom vroom
#' @export
#' @examples
#' \dontrun{
#' playerid_lookup("Garcia", "Karim")
#' }

playerid_lookup <- function(last_name = NULL, first_name = NULL) {
  if (!exists("chadwick_player_lu_table")) {
    message("Be patient, this may take a few seconds...")
    message("Data courtesy of the Chadwick Bureau Register (https://github.com/chadwickbureau/register)")
    url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
    suppressMessages(

      chadwick_player_lu_table <- vroom::vroom(url,
                                               delim = ',')
    )
    # assign("chadwick_player_lu_table", chadwick_player_lu_table, envir = .GlobalEnv)

    x <- process_player_name(last_name, first_name)

    names(x) <- c("first_name", "last_name", "given_name", "name_suffix", "nick_name", "birth_year", "mlb_played_first", "mlbam_id", "retrosheet_id", "bbref_id", "fangraphs_id")
	
    return(x)
	
  }

  else {
    x <- process_player_name(last_name, first_name)

    names(x) <- c("first_name", "last_name", "given_name", "name_suffix", "nick_name", "birth_year", "mlb_played_first", "mlbam_id", "retrosheet_id", "bbref_id", "fangraphs_id")
    x$fangraphs_id <- as.character(x$fangraphs_id) %>% as.numeric()
    x$birth_year <- as.character(x$birth_year) %>% as.numeric()
    x
  }
}


process_player_name <- function(last_name = NULL, first_name = NULL) {
  if (!exists("chadwick_player_lu_table")) {
    print("Be patient, this may take a few seconds...")
    print("Data courtesy of the Chadwick Bureau Register (https://github.com/chadwickbureau/register)")
    url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
    chadwick_player_lu_table <- readr::read_csv(url)
    # assign("chadwick_player_lu_table", chadwick_player_lu_table, envir = .GlobalEnv)
  }
  if (is.null(first_name)) {
    x <- chadwick_player_lu_table %>%
      dplyr::filter(grepl(last_name, .data$name_last)) %>%
      dplyr::select(.data$name_first, .data$name_last, .data$name_given, .data$name_suffix, 
                    .data$name_nick, .data$birth_year, .data$mlb_played_first, .data$key_mlbam, 
                    .data$key_retro, .data$key_bbref, .data$key_fangraphs)
  }
  else {
    x <- chadwick_player_lu_table %>%
      dplyr::filter(grepl(last_name, .data$name_last)) %>%
      dplyr::filter(grepl(first_name, .data$name_first)) %>%
      dplyr::select(.data$name_first, .data$name_last, .data$name_given, .data$name_suffix, 
                    .data$name_nick, .data$birth_year, .data$mlb_played_first, 
                    .data$key_mlbam, .data$key_retro, .data$key_bbref, .data$key_fangraphs)
  }
  return(x)
}

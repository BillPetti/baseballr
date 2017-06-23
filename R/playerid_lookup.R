#' Look up Baseball Player IDs
#'
#' This function allows you to query the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' @param last_name A text string used to return results for players with that string in their last name.
#' @param first_name A text string used to return results for players with that string in their first name.
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' \dontrun{playerid_lookup("Garcia", "Karim")}

playerid_lookup <- function(last_name=NULL, first_name=NULL) {
  if (!exists("chadwick_player_lu_table")) {
    print("Be patient, this may take a few seconds...")
    print("Data courtesy of the Chadwick Bureau Register (https://github.com/chadwickbureau/register)")
    url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
    chadwick_player_lu_table <- read.csv(url)
    assign("chadwick_player_lu_table", chadwick_player_lu_table, envir = .GlobalEnv)
    
    x <- process_player_name(last_name, first_name)
    
    names(x) <- c("first_name", "last_name", "given_name", "name_suffix", "nick_name", "birth_year", "mlb_played_first", "mlbam_id", "retrosheet_id", "bbref_id", "fangraphs_id")
    
    x$fangraphs_id <- as.character(x$fangraphs_id) %>% as.numeric()
    x$birth_year <- as.character(x$birth_year) %>% as.numeric()
    x
  }
  
  else {
    x <- process_player_name(last_name, first_name)
    
    names(x) <- c("first_name", "last_name", "given_name", "name_suffix", "nick_name", "birth_year", "mlb_played_first", "mlbam_id", "retrosheet_id", "bbref_id", "fangraphs_id")
    x$fangraphs_id <- as.character(x$fangraphs_id) %>% as.numeric()
    x$birth_year <- as.character(x$birth_year) %>% as.numeric()
    x
  }
}

process_player_name <- function(last_name=NULL, first_name=NULL) {
  if (is.null(first_name)) {
    x <- chadwick_player_lu_table %>%
      filter(grepl(last_name, name_last)) %>%
      select(name_first, name_last, name_given, name_suffix, name_nick, birth_year, mlb_played_first, key_mlbam, key_retro, key_bbref, key_fangraphs)
  }
  else {
    x <- chadwick_player_lu_table %>%
      filter(grepl(last_name, name_last)) %>%
      filter(grepl(first_name, name_first)) %>%
      select(name_first, name_last, name_given, name_suffix, name_nick, birth_year, mlb_played_first, key_mlbam, key_retro, key_bbref, key_fangraphs)
  }
  x
}
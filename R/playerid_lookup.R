#' Look up Baseball Player IDs
#'
#' This function allows you to query the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' @param string A text string used to return results for players with that string in their last name.
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' \dontrun{playerid_lookup("Synder")}

playerid_lookup <- function(string=NULL) {
  if (is.null(string)) {
    return("You must provide a string of text to match player records to.")
    }

  print("Be patient, this may take a few seconds...")
  print("Data courtesy of the Chadwick Bureau Register (https://github.com/chadwickbureau/register)")
  name <- string
  url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
  x <- read.csv(url) %>%
  filter(grepl(name, name_last)) %>%
    select(name_first, name_last, name_given, name_suffix, name_nick, birth_year, mlb_played_first, key_mlbam, key_retro, key_bbref, key_fangraphs)
  names(x) <- c("first_name", "last_name", "given_name", "name_suffix", "nick_name", "birth_year", "mlb_played_first", "mlbam_id", "retrosheet_id", "bbref_id", "fangraphs_id")
  x$fangraphs_id <- as.character(x$fangraphs_id) %>% as.numeric()
  x$birth_year <- as.character(x$birth_year) %>% as.numeric()
  x
}

#' Look up Baseball Player Name
#'
#' This function allows you to query the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' @param id An integer or character string representing a player ID in a baseball database, cross-referenced through the Chadwick Bureau's public register of baseball players.
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' \dontrun{playername_lookup(4885)}
#' \dontrun{playername_lookup("kaaihki01")}

playername_lookup <- function(id) {
  if (!exists("chadwick_player_lu_table")) {
    print("Be patient, this may take a few seconds...")
    print("Data courtesy of the Chadwick Bureau Register (https://github.com/chadwickbureau/register)")
    url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
    chadwick_player_lu_table <- read.csv(url)
    assign("chadwick_player_lu_table", chadwick_player_lu_table, envir = .GlobalEnv)
  }
  
  x <- chadwick_player_lu_table %>% 
    filter(id == key_mlbam | id == key_retro | id == key_bbref | id == key_fangraphs) %>%
    select(name_first, name_last, name_given, name_suffix, name_nick, birth_year, mlb_played_first, key_mlbam, key_retro, key_bbref, key_fangraphs)
  x
}

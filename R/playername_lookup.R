#' Look up Baseball Player Name
#'
#' This function allows you to query the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' @param id An integer or character string representing a player ID in a baseball database, cross-referenced through the Chadwick Bureau's public register of baseball players.
#' @export
#' @examples
#' \dontrun{playername_lookup(4885)}
#' \dontrun{playername_lookup("kaaihki01")}

playername_lookup <- function(id) {
  if (!exists("chadwick_player_lu_table")) {
    print("Be patient, this may take a few seconds...")
    print("Data courtesy of the Chadwick Bureau Register (https://github.com/chadwickbureau/register)")
    url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
    chadwick_player_lu_table <- readr::read_csv(url)
    # assign("chadwick_player_lu_table", chadwick_player_lu_table, envir = .GlobalEnv)
  }
  
  x <- chadwick_player_lu_table %>% 
    dplyr::filter(id == .data$key_mlbam | id == .data$key_retro | id == .data$key_bbref | id == .data$key_fangraphs) %>%
    dplyr::select(.data$name_first, .data$name_last, .data$name_given, .data$name_suffix,
                  .data$name_nick, .data$birth_year, .data$mlb_played_first, .data$key_mlbam, 
                  .data$key_retro, .data$key_bbref, .data$key_fangraphs)
  x
}

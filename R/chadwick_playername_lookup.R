#' @rdname chadwick_player_name_lu
#' @title **Look up Baseball Player Name by ID**
#' @description This function allows you to query the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' @param id An integer or character string representing a player ID in a baseball database, cross-referenced through the Chadwick Bureau's public register of baseball players.
#' @return A data frame of baseball players and the various IDs associated with them in different systems of record.
#'
#'  |col_name         |types     |description |
#'  |:----------------|:---------|:-----------|
#'  |name_first       |character |Player first name. |
#'  |name_last        |character |Player last name. |
#'  |name_given       |character |Player full given (legal) name. |
#'  |name_suffix      |character |Name suffix (e.g. Jr., Sr., III). |
#'  |name_nick        |character |Player nickname. |
#'  |birth_year       |integer   |Year of birth. |
#'  |mlb_played_first |integer   |First MLB season as a player. |
#'  |key_mlbam        |integer   |MLB Advanced Media (MLBAM) player ID. |
#'  |key_retro        |character |Retrosheet player ID. |
#'  |key_bbref        |character |Baseball-Reference player ID. |
#'  |key_fangraphs    |integer   |FanGraphs player ID. |
#'
#' @export
#' @examples \donttest{
#'   try(playername_lookup(4885))
#'   try(playername_lookup("kaaihki01"))
#' }

playername_lookup <- function(id) {
  if (!exists("chadwick_player_lu_table")) {
    chadwick_player_lu_table <- chadwick_player_lu()
  }
  
  x <- chadwick_player_lu_table |> 
    dplyr::filter(id == .data$key_mlbam | id == .data$key_retro | id == .data$key_bbref | id == .data$key_fangraphs) |>
    dplyr::select("name_first", "name_last", "name_given", "name_suffix",
                  "name_nick", "birth_year", "mlb_played_first", "key_mlbam", 
                  "key_retro", "key_bbref", "key_fangraphs")
  x <- x |>
    make_baseballr_data("Player Name Lookup from the Chadwick Bureau's public register of baseball players",Sys.time())
  return(x)
}

#' @rdname chadwick_player_id_lu
#' @title **Look up Baseball Player IDs by Player Name**
#'
#' @description This function allows you to query the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' @param last_name A text string used to return results for players with that string in their last name.
#' @param first_name A text string used to return results for players with that string in their first name.
#' @return A data frame of baseball players and the various IDs associated with them in different systems of record.
#'   |col_name         |types     |
#'   |:----------------|:---------|
#'   |first_name       |character |
#'   |last_name        |character |
#'   |given_name       |character |
#'   |name_suffix      |character |
#'   |nick_name        |character |
#'   |birth_year       |integer   |
#'   |mlb_played_first |integer   |
#'   |mlbam_id         |integer   |
#'   |retrosheet_id    |character |
#'   |bbref_id         |character |
#'   |fangraphs_id     |integer   |
#' @export
#' @examples \donttest{
#'   try(playerid_lookup("Garcia", "Karim"))
#' }

playerid_lookup <- function(last_name = NULL, first_name = NULL) {
  if (!exists("chadwick_player_lu_table")) {
    
    chadwick_player_lu_table <- chadwick_player_lu()
    x <- process_player_name(last_name, first_name, chadwick_player_lu_table)

    names(x) <- c("first_name", "last_name", "given_name", "name_suffix", "nick_name", "birth_year", "mlb_played_first", "mlbam_id", "retrosheet_id", "bbref_id", "fangraphs_id")
	
    x <- x %>%
      make_baseballr_data("Player ID Lookup from the Chadwick Bureau's public register of baseball players",Sys.time())
    return(x)
	
  }

  else {
    x <- process_player_name(last_name, first_name, chadwick_player_lu_table)
    
    names(x) <- c("first_name", "last_name", "given_name", "name_suffix", "nick_name", "birth_year", "mlb_played_first", "mlbam_id", "retrosheet_id", "bbref_id", "fangraphs_id")
    suppressWarnings(
      x$fangraphs_id <- x$fangraphs_id %>% 
        as.character() %>% 
        as.numeric()
    )
    suppressWarnings(
      x$birth_year <- x$birth_year %>% 
        as.character() %>% 
        as.numeric()
    )
    x <- x %>%
      make_baseballr_data("Player ID Lookup from the Chadwick Bureau's public register of baseball players",Sys.time())
    return(x)
  }
}


process_player_name <- function(last_name = NULL, first_name = NULL, chadwick_player_lu_table = NULL) {
  if (is.null(chadwick_player_lu_table)) {
    chadwick_player_lu_table <- chadwick_player_lu()
  }
  if (is.null(first_name)) {
    x <- chadwick_player_lu_table %>%
      dplyr::filter(grepl(last_name, .data$name_last)) %>%
      dplyr::select("name_first", "name_last", "name_given", "name_suffix", 
                    "name_nick", "birth_year", "mlb_played_first", "key_mlbam", 
                    "key_retro", "key_bbref", "key_fangraphs")
  }
  else {
    x <- chadwick_player_lu_table %>%
      dplyr::filter(grepl(last_name, .data$name_last)) %>%
      dplyr::filter(grepl(first_name, .data$name_first)) %>%
      dplyr::select("name_first", "name_last", "name_given", "name_suffix", 
                    "name_nick", "birth_year", "mlb_played_first", 
                    "key_mlbam", "key_retro", "key_bbref", "key_fangraphs")
  }
  return(x)
}

#' @rdname chadwick_player_id_lu
#' @title **Look up Baseball Player IDs by Player Name**
#'
#' @description This function allows you to query the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' @param last_name A text string used to return results for players with that string in their last name.
#' @param first_name A text string used to return results for players with that string in their first name.
#' @export
#' @examples \donttest{
#'   playerid_lookup("Garcia", "Karim")
#' }

playerid_lookup <- function(last_name = NULL, first_name = NULL) {
  if (!exists("chadwick_player_lu_table")) {
    
    chadwick_player_lu_table <- chadwick_player_lu()
    x <- process_player_name(last_name, first_name, chadwick_player_lu_table)

    names(x) <- c("first_name", "last_name", "given_name", "name_suffix", "nick_name", "birth_year", "mlb_played_first", "mlbam_id", "retrosheet_id", "bbref_id", "fangraphs_id")
	
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

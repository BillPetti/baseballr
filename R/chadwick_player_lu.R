#' @rdname chadwick_player_lu
#' @title **Download the Chadwick Bureau's public register of baseball players**
#' @return A data frame of baseball players and the various IDs associated with them in different systems of record 
#' and the following columns: 
#' 
#'   |col_name          |types     |
#'   |:-----------------|:---------|
#'   |key_person        |character |
#'   |key_uuid          |character |
#'   |key_mlbam         |integer   |
#'   |key_retro         |character |
#'   |key_bbref         |character |
#'   |key_bbref_minors  |character |
#'   |key_fangraphs     |integer   |
#'   |key_npb           |integer   |
#'   |key_sr_nfl        |character |
#'   |key_sr_nba        |character |
#'   |key_sr_nhl        |character |
#'   |key_findagrave    |integer   |
#'   |name_last         |character |
#'   |name_first        |character |
#'   |name_given        |character |
#'   |name_suffix       |character |
#'   |name_matrilineal  |character |
#'   |name_nick         |character |
#'   |birth_year        |integer   |
#'   |birth_month       |integer   |
#'   |birth_day         |integer   |
#'   |death_year        |integer   |
#'   |death_month       |integer   |
#'   |death_day         |integer   |
#'   |pro_played_first  |integer   |
#'   |pro_played_last   |integer   |
#'   |mlb_played_first  |integer   |
#'   |mlb_played_last   |integer   |
#'   |col_played_first  |integer   |
#'   |col_played_last   |integer   |
#'   |pro_managed_first |integer   |
#'   |pro_managed_last  |integer   |
#'   |mlb_managed_first |integer   |
#'   |mlb_managed_last  |integer   |
#'   |col_managed_first |integer   |
#'   |col_managed_last  |integer   |
#'   |pro_umpired_first |integer   |
#'   |pro_umpired_last  |integer   |
#'   |mlb_umpired_first |integer   |
#'   |mlb_umpired_last  |integer   |
#'   
#' @export
#' @examples \donttest{
#'   try(chadwick_player_lu())
#' }
chadwick_player_lu <- function() {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  
  loader <- csv_from_url
  
  hex_seq <- c(0:9, letters[1:6])
  suppressWarnings(
    urls <- paste0("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-", hex_seq,".csv")
  )
  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = hex_seq)
  
  df <- lapply(urls, progressively(loader, p))
  df <- rbindlist_with_attrs(df)
  class(df) <- c("baseballr_data","tbl_df","tbl","data.table","data.frame")
  
  
  df <- df %>%
    make_baseballr_data("Player Lookup from the Chadwick Bureau's public register of baseball players",Sys.time())
  return(df)
}
#' @rdname chadwick_player_lu
#' @return A data frame of baseball players and the various IDs associated with them in different systems of record.
#' @export
get_chadwick_lu <-  chadwick_player_lu

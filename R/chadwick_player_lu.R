#' @rdname chadwick_player_lu
#' @title **Download the Chadwick Bureau's public register of baseball players**
#' @return A data frame of baseball players and the various IDs associated with them in different systems of record 
#' and the following columns: 
#' 
#'   |col_name          |types     |description |
#'   |:-----------------|:---------|:-----------|
#'   |key_person        |character |Chadwick Bureau primary person key. |
#'   |key_uuid          |character |Chadwick Bureau universally unique identifier for the person. |
#'   |key_mlbam         |integer   |MLB Advanced Media (MLBAM) player ID. |
#'   |key_retro         |character |Retrosheet player ID. |
#'   |key_bbref         |character |Baseball-Reference major league player ID. |
#'   |key_bbref_minors  |character |Baseball-Reference minor league player ID. |
#'   |key_fangraphs     |integer   |FanGraphs player ID. |
#'   |key_npb           |integer   |Nippon Professional Baseball (Japan) player ID. |
#'   |key_sr_nfl        |character |Sports Reference NFL player ID. |
#'   |key_sr_nba        |character |Sports Reference NBA player ID. |
#'   |key_sr_nhl        |character |Sports Reference NHL player ID. |
#'   |key_wikidata      |character |Wikidata entity ID for the person. |
#'   |name_last         |character |Player last name. |
#'   |name_first        |character |Player first name. |
#'   |name_given        |character |Player full given (legal) name. |
#'   |name_suffix       |character |Name suffix (e.g. Jr., Sr., III). |
#'   |name_matrilineal  |character |Maternal surname, where recorded. |
#'   |name_nick         |character |Player nickname. |
#'   |birth_year        |integer   |Year of birth. |
#'   |birth_month       |integer   |Month of birth. |
#'   |birth_day         |integer   |Day of birth. |
#'   |death_year        |integer   |Year of death. |
#'   |death_month       |integer   |Month of death. |
#'   |death_day         |integer   |Day of death. |
#'   |pro_played_first  |integer   |First season the person played professional baseball. |
#'   |pro_played_last   |integer   |Last season the person played professional baseball. |
#'   |mlb_played_first  |integer   |First MLB season as a player. |
#'   |mlb_played_last   |integer   |Last MLB season as a player. |
#'   |col_played_first  |integer   |First college season as a player. |
#'   |col_played_last   |integer   |Last college season as a player. |
#'   |pro_managed_first |integer   |First professional season as a manager. |
#'   |pro_managed_last  |integer   |Last professional season as a manager. |
#'   |mlb_managed_first |integer   |First MLB season as a manager. |
#'   |mlb_managed_last  |integer   |Last MLB season as a manager. |
#'   |col_managed_first |integer   |First college season as a manager. |
#'   |col_managed_last  |integer   |Last college season as a manager. |
#'   |pro_umpired_first |integer   |First professional season as an umpire. |
#'   |pro_umpired_last  |integer   |Last professional season as an umpire. |
#'   |mlb_umpired_first |integer   |First MLB season as an umpire. |
#'   |mlb_umpired_last  |integer   |Last MLB season as an umpire. |
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
  
  
  df <- df |>
    make_baseballr_data("Player Lookup from the Chadwick Bureau's public register of baseball players",Sys.time())
  return(df)
}
#' @rdname chadwick_player_lu
#' @return A data frame of baseball players and the various IDs associated with them in different systems of record.
#' @export
get_chadwick_lu <-  chadwick_player_lu

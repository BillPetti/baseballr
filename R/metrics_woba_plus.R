#' @rdname woba_plus
#' @title **Calculate wOBA and related metrics for any set of data**
#' @description This function allows you to calculate wOBA for any given set of data, provided the right variables are in the data set. The function currently returns both wOBA per plate appearance on wOBA per instance of fair contact.
#' @param df A data frame of statistics that includes, at a minimum, the following columns: uBB (unintentional walks), HBP (Hit By Pitch), X1B (singles), X2B (doubles), X3B (triples), HR (home runs), AB (at-bats), SH (sacrifice hits), SO (strike outs), and season.
#' @return Returns a tibble with the wOBA factors calculated and the following columns:
#'   
#'    |col_name |types     |
#'    |:--------|:---------|
#'    |bbref_id |character |
#'    |season   |integer   |
#'    |Name     |character |
#'    |Age      |numeric   |
#'    |Level    |character |
#'    |Team     |character |
#'    |G        |numeric   |
#'    |PA       |numeric   |
#'    |AB       |numeric   |
#'    |R        |numeric   |
#'    |H        |numeric   |
#'    |X1B      |numeric   |
#'    |X2B      |numeric   |
#'    |X3B      |numeric   |
#'    |HR       |numeric   |
#'    |RBI      |numeric   |
#'    |BB       |numeric   |
#'    |IBB      |numeric   |
#'    |uBB      |numeric   |
#'    |SO       |numeric   |
#'    |HBP      |numeric   |
#'    |SH       |numeric   |
#'    |SF       |numeric   |
#'    |GDP      |numeric   |
#'    |SB       |numeric   |
#'    |CS       |numeric   |
#'    |BA       |numeric   |
#'    |OBP      |numeric   |
#'    |SLG      |numeric   |
#'    |OPS      |numeric   |
#'    |wOBA     |numeric   |
#'    |wOBA_CON |numeric   |
#'   
#' @importFrom dplyr left_join desc arrange
#' @import rvest 
#' @export
#' @examples \donttest{
#'  try({
#'    df <- bref_daily_batter("2015-08-01", "2015-10-03") 
#'    woba_plus(df)
#'  })
#' }
woba_plus <- function(df) {
  
  df$season <- as.integer(df$season)
  
  if (!exists("guts_table")) {
    guts_table <- fg_guts()

  }

  df_join <- df %>% 
    dplyr::left_join(guts_table, by = "season")

  df_join$wOBA <- round((((df_join$wBB * df_join$uBB) +
                            (df_join$wHBP * df_join$HBP) +
                            (df_join$w1B * df_join$X1B) +
                            (df_join$w2B * df_join$X2B) +
                            (df_join$w3B * df_join$X3B) +
                            (df_join$wHR * df_join$HR))/
                           (df_join$AB + df_join$uBB + df_join$HBP + df_join$SF)),3)

  df_join$wOBA_CON <- round((((df_join$w1B * df_join$X1B) +
                                (df_join$w2B * df_join$X2B) +
                                (df_join$w3B * df_join$X3B) +
                                (df_join$wHR * df_join$HR))/
                               (df_join$AB - df_join$SO)),3)

  df_join <- df_join %>% 
    dplyr::arrange(dplyr::desc(.data$wOBA))
  x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  df_join <- df_join[!x]

  return(df_join)

}

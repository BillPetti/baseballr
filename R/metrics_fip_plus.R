#' @rdname fip_plus
#' @title **Calculate FIP and related metrics for any set of data**
#' @description This function allows you to calculate FIP and related metrics for any given set of data, provided the right variables are in the data set. The function currently returns both FIP per inning pitched, wOBA against (based on batters faced), and wOBA against per instance of fair contact.
#' @param df A data frame of statistics that includes, at a minimum, the following columns: IP (innings pitched), BF (batters faced), uBB (unintentional walks), HBP (Hit By Pitch), x1B (singles), x2B (doubles), x3B (triples), HR (home runs), AB (at-bats), SH (sacrifice hits), SO (strike outs), and season.
#' @return Returns a tibble with the following columns:
#' 
#'   |col_name         |types     |
#'   |:----------------|:---------|
#'   |bbref_id         |character |
#'   |season           |integer   |
#'   |Name             |character |
#'   |Age              |numeric   |
#'   |Level            |character |
#'   |Team             |character |
#'   |G                |numeric   |
#'   |GS               |numeric   |
#'   |W                |numeric   |
#'   |L                |numeric   |
#'   |SV               |numeric   |
#'   |IP               |numeric   |
#'   |H                |numeric   |
#'   |R                |numeric   |
#'   |ER               |numeric   |
#'   |uBB              |numeric   |
#'   |BB               |numeric   |
#'   |SO               |numeric   |
#'   |HR               |numeric   |
#'   |HBP              |numeric   |
#'   |ERA              |numeric   |
#'   |AB               |numeric   |
#'   |X1B              |numeric   |
#'   |X2B              |numeric   |
#'   |X3B              |numeric   |
#'   |IBB              |numeric   |
#'   |GDP              |numeric   |
#'   |SF               |numeric   |
#'   |SB               |numeric   |
#'   |CS               |numeric   |
#'   |PO               |numeric   |
#'   |BF               |numeric   |
#'   |Pit              |numeric   |
#'   |Str              |numeric   |
#'   |StL              |numeric   |
#'   |StS              |numeric   |
#'   |GB.FB            |numeric   |
#'   |LD               |numeric   |
#'   |PU               |numeric   |
#'   |WHIP             |numeric   |
#'   |BAbip            |numeric   |
#'   |SO9              |numeric   |
#'   |SO.W             |numeric   |
#'   |SO_perc          |numeric   |
#'   |uBB_perc         |numeric   |
#'   |SO_uBB           |numeric   |
#'   |FIP              |numeric   |
#'   |wOBA_against     |numeric   |
#'   |wOBA_CON_against |numeric   |
#'   
#' @importFrom dplyr left_join desc arrange
#' @import rvest
#' @export
#' @examples \donttest{
#'   try({
#'     df <- bref_daily_pitcher("2015-04-05", "2015-04-30")
#'     fip_plus(df)
#'   })
#' }

fip_plus <- function(df) {
  
  df$season <- as.integer(df$season)
  
  if (!exists("guts_table")) {
    guts_table <- fg_guts()
  }

  df_join <- df %>% 
    dplyr::left_join(guts_table, by = "season")
  df_join$FIP <- round(((((13*df_join$HR) + (3*(df_join$uBB + df_join$HBP)) - (2*df_join$SO))/df_join$IP) + df_join$cFIP), 2)
  df_join$wOBA_against <- round((((df_join$wBB * df_join$uBB) + (df_join$wHBP * df_join$HBP) + (df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/(df_join$BF)),3)
  df_join$wOBA_CON_against <- round((((df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/(df_join$AB - df_join$SO)),3)
  df_join <- df_join %>% 
    dplyr::arrange(dplyr::desc(.data$wOBA_against))
  x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  df_join <- df_join[!x]
  return(df_join)
}

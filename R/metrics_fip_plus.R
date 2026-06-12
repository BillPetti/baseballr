#' @rdname fip_plus
#' @title **Calculate FIP and related metrics for any set of data**
#' @description This function allows you to calculate FIP and related metrics for any given set of data, provided the right variables are in the data set. The function currently returns both FIP per inning pitched, wOBA against (based on batters faced), and wOBA against per instance of fair contact.
#' @param df A data frame of statistics that includes, at a minimum, the following columns: IP (innings pitched), BF (batters faced), uBB (unintentional walks), HBP (Hit By Pitch), x1B (singles), x2B (doubles), x3B (triples), HR (home runs), AB (at-bats), SH (sacrifice hits), SO (strike outs), and season.
#' @return Returns a tibble with the following columns:
#' 
#'   |col_name         |types     |description                                              |
#'   |:----------------|:---------|:--------------------------------------------------------|
#'   |bbref_id         |character |Baseball-Reference player id                             |
#'   |season           |integer   |Season (4-digit year)                                    |
#'   |Name             |character |Player name                                              |
#'   |Age              |numeric   |Player age                                               |
#'   |Level            |character |Level of play (e.g. MLB or minor-league level)           |
#'   |Team             |character |Team name                                                |
#'   |G                |numeric   |Games                                                    |
#'   |GS               |numeric   |Games started                                            |
#'   |W                |numeric   |Wins                                                     |
#'   |L                |numeric   |Losses                                                   |
#'   |SV               |numeric   |Saves                                                    |
#'   |IP               |numeric   |Innings pitched                                          |
#'   |H                |numeric   |Hits allowed                                             |
#'   |R                |numeric   |Runs allowed                                             |
#'   |ER               |numeric   |Earned runs allowed                                      |
#'   |uBB              |numeric   |Unintentional walks allowed                              |
#'   |BB               |numeric   |Walks (bases on balls) allowed                           |
#'   |SO               |numeric   |Strikeouts                                               |
#'   |HR               |numeric   |Home runs allowed                                        |
#'   |HBP              |numeric   |Hit batters                                              |
#'   |ERA              |numeric   |Earned run average                                       |
#'   |AB               |numeric   |At-bats against                                          |
#'   |X1B              |numeric   |Singles allowed                                          |
#'   |X2B              |numeric   |Doubles allowed                                          |
#'   |X3B              |numeric   |Triples allowed                                          |
#'   |IBB              |numeric   |Intentional walks allowed                                |
#'   |GDP              |numeric   |Grounded into double plays induced                       |
#'   |SF               |numeric   |Sacrifice flies allowed                                  |
#'   |SB               |numeric   |Stolen bases allowed                                     |
#'   |CS               |numeric   |Caught stealing                                          |
#'   |PO               |numeric   |Pickoffs                                                 |
#'   |BF               |numeric   |Batters faced                                            |
#'   |Pit              |numeric   |Pitches thrown                                           |
#'   |Str              |numeric   |Strikes thrown                                           |
#'   |StL              |numeric   |Strikes looking (called)                                 |
#'   |StS              |numeric   |Strikes swinging (whiffs)                                |
#'   |GB.FB            |numeric   |Ground ball to fly ball ratio                            |
#'   |LD               |numeric   |Line drives allowed                                      |
#'   |PU               |numeric   |Pop ups (infield fly) induced                            |
#'   |WHIP             |numeric   |Walks plus hits per inning pitched                       |
#'   |BAbip            |numeric   |Batting average on balls in play allowed                 |
#'   |SO9              |numeric   |Strikeouts per nine innings                              |
#'   |SO.W             |numeric   |Strikeout-to-walk ratio                                  |
#'   |SO_perc          |numeric   |Strikeout percentage                                     |
#'   |uBB_perc         |numeric   |Unintentional walk percentage                            |
#'   |SO_uBB           |numeric   |Strikeout percentage minus unintentional walk percentage |
#'   |FIP              |numeric   |Fielding independent pitching                            |
#'   |wOBA_against     |numeric   |Weighted on-base average against (based on batters faced)|
#'   |wOBA_CON_against |numeric   |Weighted on-base average against per instance of fair contact|
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

  df_join <- df |> 
    dplyr::left_join(guts_table, by = "season")
  df_join$FIP <- round(((((13*df_join$HR) + (3*(df_join$uBB + df_join$HBP)) - (2*df_join$SO))/df_join$IP) + df_join$cFIP), 2)
  df_join$wOBA_against <- round((((df_join$wBB * df_join$uBB) + (df_join$wHBP * df_join$HBP) + (df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/(df_join$BF)),3)
  df_join$wOBA_CON_against <- round((((df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/(df_join$AB - df_join$SO)),3)
  df_join <- df_join |> 
    dplyr::arrange(dplyr::desc(.data$wOBA_against))
  x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  df_join <- df_join[!x]
  return(df_join)
}

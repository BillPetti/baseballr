#' Scrape Batter Performance Data Over a Custom Time Frame
#'
#' This function allows you to scrape basic batter statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param t1 First date data should be scraped from. Should take the form "YEAR-DAY-MONTH"
#' @param t2 Last date data should be scraped from. Should take the form "YEAR-DAY-MONTH"
#' @keywords MLB, sabermetrics
#' @importFrom dplyr "%>%" arrange_ filter_
#' @importFrom rvest html_table html_nodes
#' @importFrom xml2 read_html
#' @export
#' @examples
#' daily_batter_bref("2015-05-10", "2015-06-20")

daily_batter_bref <- function(t1, t2) {
  df <- read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=b&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0"))
  df <- df %>% html_nodes(xpath = '//*[@id="daily"]') %>% html_table(fill = TRUE)
  df <- as.data.frame(df)[-c(1,3,5)]
  names(df)[1:4] <- c("Name", "Age", "Level", "Team")
  df[,c(2,5:26)] <- lapply(df[,c(2,5:26)],as.numeric)
  df$X1B <- with(df, H-(X2B+X3B+HR))
  season <- substr(t1, 1, 4)
  df$season <- season
  df$uBB <- with(df, BB-IBB)
  df <- df[,c(28,1:9, 27, 10:15, 29, 16:26)]
  df$Team <- gsub(" $", "", df$Team, perl=T)
  df <- filter_(df, ~Name != "Name")
  df <- arrange_(df, ~desc(PA), ~desc(OPS))
  df
}

#' Scrape Pitcher Performance Data Over a Custom Time Frame
#'
#' This function allows you to scrape basic pitcher statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param t1 First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param t2 Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_attr
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' \dontrun{daily_pitcher_bref("2015-05-10", "2015-06-20")}

daily_pitcher_bref <- function(t1, t2) {

  message('Data courtesy of Baseball-Reference.com. Please consider supporting Baseball-Reference by signing up for a Statehead account: https://stathead.com')

  payload <- read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=p&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0"))
  df <- payload %>%
    html_nodes(xpath = '//*[@id="daily"]') %>%
    html_table(fill = TRUE)

  df <- as.data.frame(df)[-c(1,3,5)]
  names(df)[1:4] <- c("Name", "Age", "Level", "Team")
  df[,c(2,5:29, 36:39)] <- lapply(df[,c(2,5:29, 36:39)], as.numeric)
  df$X1B <- with(df, H-(X2B+X3B+HR))
  season <- substr(t1, 1, 4)
  df$season <- season
  df$uBB <- with(df, BB-IBB)
  df[,30] <- as.numeric(sub("%", "", df[, 30]))
  df[,31] <- as.numeric(sub("%", "", df[, 31]))
  df[,32] <- as.numeric(sub("%", "", df[, 32]))
  df[,33] <- as.numeric(sub("%", "", df[, 33]))
  df[,34] <- as.numeric(sub("%", "", df[, 34]))
  df[,35] <- as.numeric(sub("%", "", df[, 35]))
  df[,c(30:35)] <- df[,c(30:35)]/100
  df <- df[,c(41,1:13,42,14:19,40,20:39)]
  df$SO_perc <- with(df, round(SO/BF,3))
  df$uBB_perc <- with(df, round(uBB/BF,3))
  df$SO_uBB <- with(df, round(SO_perc - uBB_perc))
  df$Team <- gsub(" $", "", df$Team, perl=T)
  df <- filter_(df, ~Name != "Name")
  df <- arrange_(df, ~desc(IP), ~desc(WHIP))

  playerids <- payload %>%
    html_nodes("table") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    rename(slug = ".") %>%
    filter(grepl("redirect", slug)) %>%
    mutate(playerid = gsub("/redirect.fcgi\\?player=1&mlb_ID=", "", slug))

  df <- df %>%
    mutate(bbref_id = playerids$playerid) %>%
    select(bbref_id, everything())

  df

}

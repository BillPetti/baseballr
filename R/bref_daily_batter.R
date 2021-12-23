#' @rdname bref_daily_batter
#' @title Scrape Batter Performance Data Over a Custom Time Frame
#'
#' @description This function allows you to scrape basic batter statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param t1 First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param t2 Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate arrange desc rename select
#' @importFrom tidyr everything
#' @import rvest 
#' @export
#' @examples \donttest{
#'   bref_daily_batter("2015-05-10", "2015-06-20")
#' }

bref_daily_batter <- function(t1, t2) {

  payload <- xml2::read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=b&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0"))
  df <- payload %>%
    rvest::html_elements(xpath = '//*[@id="daily"]') %>%
    rvest::html_table(fill = TRUE)

  df <- as.data.frame(df)[-c(1,3,5)]
  names(df)[1:4] <- c("Name", "Age", "Level", "Team")
  suppressWarnings(
    df[,c(2,5:26)] <- lapply(df[,c(2,5:26)],as.numeric)
  )
  df$X1B <- with(df, H-(X2B+X3B+HR))
  season <- substr(t1, 1, 4)
  df$season <- season
  df$uBB <- with(df, BB-IBB)
  df <- df[,c(28,1:9, 27, 10:15, 29, 16:26)]
  df$Team <- gsub(" $", "", df$Team, perl=T)
  df <- df %>% 
    dplyr::filter(.data$Name != "Name")
  df <- df %>% 
    dplyr::arrange(desc(.data$PA), desc(.data$OPS))

  playerids <- payload %>%
    rvest::html_elements("table") %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    as.data.frame() %>%
    dplyr::rename(slug = ".") %>%
    dplyr::filter(grepl("redirect", .data$slug)) %>%
    dplyr::mutate(playerid = gsub("/redirect.fcgi\\?player=1&mlb_ID=", "", .data$slug))

  df <- df %>%
    dplyr::mutate(bbref_id = playerids$playerid) %>%
    dplyr::select(.data$bbref_id, tidyr::everything())

  df
}


#' @rdname bref_daily_batter
#' @title Scrape Batter Performance Data Over a Custom Time Frame
#'
#' @description This function allows you to scrape basic batter statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param t1 First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param t2 Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @export

daily_batter_bref <- function(t1, t2) {
  
  bref_daily_batter(t1 = t1, t2 = t2)
  
}

#' @rdname bref_daily_pitcher
#' @title **Scrape Pitcher Performance Data Over a Custom Time Frame**
#'
#' @description This function allows you to scrape basic pitcher statistics over a custom time frame. Data is sourced from Baseball-Reference.com.
#' @param t1 First date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @param t2 Last date data should be scraped from. Should take the form "YEAR-MONTH-DAY"
#' @return Returns a tibble of pitcher performance with the following columns: 
#' 
#'  |col_name |types     |
#'  |:--------|:---------|
#'  |bbref_id |character |
#'  |season   |integer   |
#'  |Name     |character |
#'  |Age      |numeric   |
#'  |Level    |character |
#'  |Team     |character |
#'  |G        |numeric   |
#'  |GS       |numeric   |
#'  |W        |numeric   |
#'  |L        |numeric   |
#'  |SV       |numeric   |
#'  |IP       |numeric   |
#'  |H        |numeric   |
#'  |R        |numeric   |
#'  |ER       |numeric   |
#'  |uBB      |numeric   |
#'  |BB       |numeric   |
#'  |SO       |numeric   |
#'  |HR       |numeric   |
#'  |HBP      |numeric   |
#'  |ERA      |numeric   |
#'  |AB       |numeric   |
#'  |X1B      |numeric   |
#'  |X2B      |numeric   |
#'  |X3B      |numeric   |
#'  |IBB      |numeric   |
#'  |GDP      |numeric   |
#'  |SF       |numeric   |
#'  |SB       |numeric   |
#'  |CS       |numeric   |
#'  |PO       |numeric   |
#'  |BF       |numeric   |
#'  |Pit      |numeric   |
#'  |Str      |numeric   |
#'  |StL      |numeric   |
#'  |StS      |numeric   |
#'  |GB.FB    |numeric   |
#'  |LD       |numeric   |
#'  |PU       |numeric   |
#'  |WHIP     |numeric   |
#'  |BAbip    |numeric   |
#'  |SO9      |numeric   |
#'  |SO.W     |numeric   |
#'  |SO_perc  |numeric   |
#'  |uBB_perc |numeric   |
#'  |SO_uBB   |numeric   |
#'  
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate arrange desc rename select
#' @importFrom tidyr everything
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(bref_daily_pitcher("2015-05-10", "2015-06-20"))
#' }

bref_daily_pitcher <- function(t1, t2) {
  
  tryCatch(
    expr = {
      payload <- paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=p&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0") %>% 
        xml2::read_html()
      
      df <- payload %>%
        rvest::html_elements(xpath = '//*[@id="daily"]') %>%
        rvest::html_table(fill = TRUE)
      
      df <- as.data.frame(df)[-c(1,3,5)]
      names(df)[1:4] <- c("Name", "Age", "Level", "Team")
      suppressWarnings(
        df[,c(2,5:29, 36:39)] <- lapply(df[,c(2,5:29, 36:39)], as.numeric)
      )
      df$X1B <- with(df, H-(X2B+X3B+HR))
      season <- substr(t1, 1, 4)
      df$season <- as.integer(season)
      df$uBB <- with(df, BB-IBB)
      
      suppressWarnings(
        df[,30] <- as.numeric(sub("%", "", df[, 30]))
      )
      suppressWarnings(
        df[,31] <- as.numeric(sub("%", "", df[, 31]))
      )
      suppressWarnings(
        df[,32] <- as.numeric(sub("%", "", df[, 32]))
      )
      suppressWarnings(
        df[,33] <- as.numeric(sub("%", "", df[, 33]))
      )
      suppressWarnings(
        df[,34] <- as.numeric(sub("%", "", df[, 34]))
      )
      suppressWarnings(
        df[,35] <- as.numeric(sub("%", "", df[, 35]))
      )
      df[,c(30:35)] <- df[,c(30:35)]/100
      df <- df[,c(41,1:13,42,14:19,40,20:39)]
      df$SO_perc <- with(df, round(SO/BF,3))
      df$uBB_perc <- with(df, round(uBB/BF,3))
      df$SO_uBB <- with(df, round(SO_perc - uBB_perc))
      df$Team <- gsub(" $", "", df$Team, perl=T)
      df <- df %>% 
        dplyr::filter(.data$Name != "Name") %>% 
        dplyr::arrange(desc(.data$IP), desc(.data$WHIP))
      
      playerids <- payload %>%
        rvest::html_elements("table") %>%
        rvest::html_elements("a") %>%
        rvest::html_attr("href") %>%
        as.data.frame() %>%
        dplyr::rename(slug = .data$`.`) %>%
        dplyr::filter(grepl("redirect", .data$slug)) %>%
        dplyr::mutate(playerid = gsub("/redirect.fcgi\\?player=1&mlb_ID=", "", .data$slug))
      
      df <- df %>%
        dplyr::mutate(bbref_id = playerids$playerid) %>%
        dplyr::select(.data$bbref_id, tidyr::everything())
      
      df <- df %>%
        make_baseballr_data("MLB Daily Pitcher data from baseball-reference.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no daily pitcher data available!"))
    },
    finally = {
    }
  )
  return(df)
}


#' @rdname daily_pitcher_bref
#' @title **(legacy) Scrape Pitcher Performance Data Over a Custom Time Frame**
#' @inheritParams bref_daily_pitcher
#' @return Returns a tibble of pitcher performance 
#' @seealso ```bref_daily_pitcher()```
#' @keywords legacy
#' @export
daily_pitcher_bref <- bref_daily_pitcher
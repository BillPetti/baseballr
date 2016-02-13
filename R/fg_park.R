#' Scrape Park Factors from FanGraphs.com
#'
#' This function allows you to scrape park factors for a given season from FanGraphs.com.
#' @param yr Season for which you want to scrape the park factors.
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' fg_park(2013)

fg_park <- function(yr) {
  factor_table <- read_html(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", yr))
  factor_table <- factor_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
  factor_table <- as.data.frame(factor_table)[-(1:4), (1:14)]
  names(factor_table) <- c("season", "home_team", "basic", "single", "double", "triple", "hr", "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP")
  for(i in c(3:14)) {
    factor_table[,i] <- as.numeric(as.character(factor_table[,i]))
  }
  factor_table
}

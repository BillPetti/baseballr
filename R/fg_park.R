#' Scrape Park Factors from FanGraphs.com
#'
#' This function allows you to scrape park factors for a given season from FanGraphs.com.
#' @param yr Season for which you want to scrape the park factors.
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' fg_park(2013)

fg_park <- function(yr) {
  read_html(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", yr)) %>% 
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>% 
    html_table %>% 
    setNames(c("season", "home_team", "basic_5yr", "3yr", "1yr", "single", "double", "triple", "hr", 
               "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP"))
  
}
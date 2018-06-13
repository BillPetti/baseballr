#' Scrape Park Factors by Handedness from FanGraphs.com
#'
#' This function allows you to scrape park factors by handedness from FanGraphs.com for a given single year.
#' @param yr Season for which you want to scrape the park factors.
#' @importFrom stats setNames
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' fg_park_hand(2013)

fg_park_hand <- function(yr) {
  read_html(paste0("http://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season=", yr)) %>%
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>% 
    html_table %>% 
    stats::setNames(c("season", "home_team", "single_as_LHH", "single_as_RHH", 
               "double_as_LHH", "double_as_RHH", "triple_as_LHH", "triple_as_RHH", 
               "hr_as_LHH", "hr_as_RHH"))
}
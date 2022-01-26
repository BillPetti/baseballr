#' @rdname fg_guts
#' @title **Scrape FanGraphs.com Guts!**
#' @description Scrape historical FanGraphs Guts! table, wOBA, FIP coefficients and constants
#' @return Returns a data frame of seasonal constants from FanGraphs
#'  |col_name   |types   |
#'  |:----------|:-------|
#'  |season     |integer |
#'  |lg_woba    |numeric |
#'  |woba_scale |numeric |
#'  |wBB        |numeric |
#'  |wHBP       |numeric |
#'  |w1B        |numeric |
#'  |w2B        |numeric |
#'  |w3B        |numeric |
#'  |wHR        |numeric |
#'  |runSB      |numeric |
#'  |runCS      |numeric |
#'  |lg_r_pa    |numeric |
#'  |lg_r_w     |numeric |
#'  |cFIP       |numeric |
#' @import rvest
#' @export
#' @examples \donttest{
#'   fg_guts()
#' }
fg_guts <- function() {
  "http://www.fangraphs.com/guts.aspx?type=cn" %>% 
    xml2::read_html() %>%
    rvest::html_element(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
    rvest::html_table() %>%
    setNames(c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
               "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
}
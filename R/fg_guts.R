#' Scrape FanGraphs.com Guts!
#'
#' This function allows you to scrape the historical, season-by-season wOBA and FIP constants and coefficients at FanGraphs.com.
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' fg_guts()

# scrape historical FanGraphs Guts! table
# wOBA and FIP coefficients and constants

fg_guts <- function() {
  read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>% 
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>% 
    html_table %>% 
    setNames(c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", 
               "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
}
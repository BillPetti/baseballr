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
  guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
  guts_table <- guts_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
  guts_table<- as.data.frame(guts_table) %>% .[-(1:2), (1:14)]
  names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  for(i in c(2:ncol(guts_table))) {
    guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
  }
  guts_table
}

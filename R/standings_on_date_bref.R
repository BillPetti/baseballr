#' Scrape MLB Standings on a Given Date
#'
#' This function allows you to scrape the standings from MLB for any date you choose.
#' @param 
#' @keywords MLB, standings
#' @export
#' @examples 
#' standings_on_date_bref()

# Divisional standings on a given date from Baseball-Reference.com

# 2 AL EAST
# 3 AL CENTRAL
# 4 AL WEST
# 5 NL EAST
# 6 NL CENTRAL
# 7 NL WEST
# 8 AL Division
# 9 NL Division

standings_on_date_bref <- function(y, m, d, division) {
  standings_lu <- data.frame(Div = c("AL EAST", "AL CENTRAL", "AL WEST", "NL EAST", "NL CENTRAL", "NL WEST", "AL DIVISION", "NL DIVISION"), num = c(2,3,4,5,6,7,8,9))
  div <- standings_lu %>% filter(Div == division) %>% .$num
  standings <- read_html(paste0("http://www.baseball-reference.com/games/standings.cgi?year=",y,"&month=",m, "&day=",d,"&submit=Submit+Date", stringsAsFactors = FALSE)) %>% html_nodes("table") %>% .[[div]] %>% html_table(fill = TRUE)
  standings
}

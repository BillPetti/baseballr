# scrape park factors by handedness from FanGraphs.com, single year
# yr = Year

fg_park_hand <- function(yr) {
  factor_table <- html(paste0("http://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season=", yr))
  factor_table <- factor_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
  factor_table <- as.data.frame(factor_table) %>% .[-(1:4), (1:10)]
  names(factor_table) <- c("season", "home_team", "single_as_LHH", "single_as_RHH", "double_as_LHH", "double_as_RHH", "triple_as_LHH", "triple_as_RHH", "hr_as_LHH", "hr_as_RHH")
  for(i in c(3:10)) {
    factor_table[,i] <- as.numeric(as.character(factor_table[,i]))
  }
  factor_table
}
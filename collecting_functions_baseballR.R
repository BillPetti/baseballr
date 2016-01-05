Collecting functions for baseballR package

# package dependencies

require(pacman)
p_load(XML, dplyr)



# scrape historical park factors from FanGraphs.com, single year

fg_park <- function(yr) {
  factor_table <- readHTMLTable(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", yr))
  park_factors <- as.data.frame(factor_table[18])
  names(park_factors) <- c("season", "home_team", "basic", "single", "double", "triple", "hr", "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP")
  for(i in c(3:14)) {
    park_factors[,i] <- as.numeric(as.character(park_factors[,i]))
  }
  park_factors
}

# scrape park factors by handedness from FanGraphs.com

fg_park_hand <- function(yr) {
  factor_table <- readHTMLTable(paste0("http://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season=", yr))
  park_factors <- as.data.frame(factor_table[18])
  names(park_factors) <- c("season", "home_team", "single_as_LHH", "single_as_RHH", "double_as_LHH", "double_as_RHH", "triple_as_LHH", "triple_as_RHH", "hr_as_LHH", "hr_as_RHH")
  for(i in c(3:10)) {
    park_factors[,i] <- as.numeric(as.character(park_factors[,i]))
  }
  park_factors
}

# scrape FanGraphs Guts! table

fg_guts <- function() {
fg_guts <- readHTMLTable("http://www.fangraphs.com/guts.aspx?type=cn")
guts_table <- as.data.frame(fg_guts[16])
names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
for(i in c(2:ncol(guts_table))) {
  guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
}
guts_table
}
####################
# Collecting functions for baseballr package
# Bill Petti
# 2016
####################

# package dependencies

require(pacman)
p_load(XML, rvest, dplyr)

### with XML
# scrape historical park factors from FanGraphs.com, single year
# yr = Year

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
# yr = Year

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

### with rvest
# scrape historical park factors from FanGraphs.com, single year
# yr = Year

fg_park <- function(yr) {
  factor_table <- html(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", yr))
  factor_table <- factor_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
  factor_table <- as.data.frame(factor_table) %>% .[-(1:4), (1:14)]
  names(factor_table) <- c("season", "home_team", "basic", "single", "double", "triple", "hr", "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP")
  for(i in c(3:14)) {
    factor_table[,i] <- as.numeric(as.character(factor_table[,i]))
  }
  factor_table
}

# scrape park factors by handedness from FanGraphs.com
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

# scrape FanGraphs Guts! table

fg_guts <- function() {
  guts_table <- html("http://www.fangraphs.com/guts.aspx?type=cn")
  guts_table <- guts_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
  guts_table<- as.data.frame(guts_table) %>% .[-(1:2), (1:14)]
  names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  for(i in c(2:ncol(guts_table))) {
    guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
  }
  guts_table
}

## FanGraphs leaderboards
# x = start year, y = end year

fg_leaders <- function(x, y) {
  leaders <- readHTMLTable(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211&season=", x, "&month=0&season1=", y, "&ind=0&team=&rost=&age=&filter=&players=")) %>% .[33] %>% as.data.frame(.) %>% .[,-1]
  c <- names(leaders) 
  c <- gsub("LeaderBoard1_dg1_ctl00.", "", .) 
  c <- gsub("[[:punct:]].", "",.)
  c <- gsub("[[:punct:]]", "_pct", .)
  names(leaders) <- c
  leaders
}






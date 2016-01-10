####################
# Collecting functions for baseballr package
# Bill Petti
# 2016
####################

# package dependencies

require(pacman)
p_load(XML, rvest, dplyr, reldist)

##### Scraping data from FanGraphs.com

### with XML
# scrape historical park factors from FanGraphs.com, single year
# yr = Year

# fg_park <- function(yr) {
#   factor_table <- readHTMLTable(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", yr))
#   park_factors <- as.data.frame(factor_table[18])
#   names(park_factors) <- c("season", "home_team", "basic", "single", "double", "triple", "hr", "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP")
#   for(i in c(3:14)) {
#     park_factors[,i] <- as.numeric(as.character(park_factors[,i]))
#   }
#   park_factors
# }

# scrape park factors by handedness from FanGraphs.com
# yr = Year

# fg_park_hand <- function(yr) {
#   factor_table <- readHTMLTable(paste0("http://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season=", yr))
#   park_factors <- as.data.frame(factor_table[18])
#   names(park_factors) <- c("season", "home_team", "single_as_LHH", "single_as_RHH", "double_as_LHH", "double_as_RHH", "triple_as_LHH", "triple_as_RHH", "hr_as_LHH", "hr_as_RHH")
#   for(i in c(3:10)) {
#     park_factors[,i] <- as.numeric(as.character(park_factors[,i]))
#   }
#   park_factors
# }

# scrape FanGraphs Guts! table

# fg_guts <- function() {
# fg_guts <- readHTMLTable("http://www.fangraphs.com/guts.aspx?type=cn")
# guts_table <- as.data.frame(fg_guts[16])
# names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
# for(i in c(2:ncol(guts_table))) {
#   guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
# }
# guts_table
# }

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
# hitters
# x = start year, y = end year, qual = qualified (min number of plate appearances, or y for qualified only players)

fg_bat_leaders <- function(x, y, qual) {
  leaders <- html(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=", qual, "&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211&season=", y, "&month=0&season1=", x, "&ind=0&team=&rost=&age=&filter=&players=&page=1_100000")) %>% html_nodes("table") %>% .[[33]] %>% html_table(fill=TRUE) %>% .[,-1]
  c <- as.matrix(names(leaders)) 
  c <- gsub("%", "_pct", c, fixed = TRUE)
  c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
  c <- gsub("/", "_", c, fixed = TRUE)
  c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
  r <- c("wRC_plus", "WPA_minus", "WPA_plus", "FB_pct", "AgeRng")
  c[c(61,63,64,74,202),] <- r
  Seasons <- ifelse(x==y, paste0(x), paste0(x, "-", y))
  names(leaders) <- c
  leaders <- cbind(Seasons, leaders)
  leaders <- as.data.frame(sapply(leaders, function(x) (gsub("\\ %", "", x))))
  leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))))
  for(i in c(3:ncol(leaders))) {
    leaders[,i] <- as.numeric(as.character(leaders[,i]))
  }
  return(leaders)
}


#### Scraping data from Baseball-Reference.com

# Scrape schedule and results for a major league team

# team_abrv <- function(tm, year) {
#   teams <- html("http://www.baseball-reference.com/leagues/MLB/2015.shtml", stringsAsFactors=FALSE) %>% html_nodes("table") %>% .[[]]
#   teams<-select(teams, Tm)
#   teams<-filter(teams, Tm!="LgAvg")
# }

team_results <-function(Tm, year) {
  data <- html(paste0("http://www.baseball-reference.com/teams/", Tm, "/", year, "-schedule-scores.shtml")) %>% html_nodes("table") %>% .[[length(.)]] %>% html_table() %>% .[,-4]
  colnames(data)[5] <- "H_A" 
  colnames(data)[7] <- "Result" 
  colnames(data)[11] <- "Record" 
  data$H_A <- ifelse(grepl("@", data$H_A, fixed = TRUE), "A", "H")
  data <- filter(data, Rk != "Rk")
  data$Attendance <- gsub(",", "", data$Attendance)
  data$Streak <- ifelse(grepl("-", data$Streak, fixed = TRUE), nchar(data$Streak) * -1, nchar(data$Streak) * 1)
  for(i in c(1,2,8,9,10,12,19)) {
    data[,i] <- as.numeric(data[,i])
  }
  data
}

# 2 AL EAST
# 3 AL CENTRAL
# 4 AL WEST
# 5 NL EAST
# 6 NL CENTRAL
# 7 NL WEST
# 8 AL Division
# 9 NL Division

standings_on_date <- function(y, m, d, division) {
  standings_lu <- data.frame(Div = c("AL EAST", "AL CENTRAL", "AL WEST", "NL EAST", "NL CENTRAL", "NL WEST", "AL DIVISION", "NL DIVISION"), num = c(2,3,4,5,6,7,8,9))
  div <- standings_lu %>% filter(Div == division) %>% .$num
  standings <- html(paste0("http://www.baseball-reference.com/games/standings.cgi?year=",y,"&month=",m, "&day=",d,"&submit=Submit+Date", stringsAsFactors = FALSE)) %>% html_nodes("table") %>% .[[div]] %>% html_table(fill = TRUE)
  standings
}


team_consistency <- function(year) {
  teams <- html(paste0("http://www.baseball-reference.com/leagues/MLB/", year, ".shtml"), stringsAsFactors=FALSE) %>% html_nodes("table") %>% .[[2]] %>% html_table()
  teams <- teams %>% select(Tm) %>% filter(Tm != "LgAvg") %>% filter(Tm != "Tm") %>% filter(!is.na(Tm))
  teams$year <- year
  teams <- teams %>% group_by(Tm, year) %>% do(team_results(.$Tm, .$year))
  teams
}
  

# #apply the scrape_results function to every team in 2015
# 
# 
# 
# #tidy up the data.frame using dplyr
# 
# cols<-c(3:5, 9:10)
# results_2015<-results_2015[,cols]
# names(results_2015)<-c("Date", "box", "Team", "R", "RA")
# #clean up some attributes left over from scraping the data from B-REF
# attr(results_2015, "vars")<-NULL
# results_2015<-filter(results_2015, box=="boxscore")
# 
# #convert R and RA columns to numeric
# 
# results_2015$R<-as.numeric(results_2015$R)
# results_2015$RA<-as.numeric(results_2015$RA)
# 
# #calculate volatility scores for each team using Gini coefficients and the reldist package
# 
# RGini<-aggregate(R ~ Team, data = results_2015, FUN = "gini")
# 
# RAGini<-aggregate(RA ~ Team, data = results_2015, FUN = "gini")
#   
# }
#   
#   
# #create function for scraping all records for all teams in 2015
# 
# # scrape_results<-function(Tm) {
# #   url <- paste0("http://www.baseball-reference.com/teams/", Tm, "/2015-schedule-scores.shtml")
# #   data <- readHTMLTable(url, stringsAsFactors = FALSE)
# #   data <- data[[6]]
# #   data
# # }
# # 
# # #apply the scrape_results function to every team in 2015
# # 
# # results_2015<-teams %>% group_by(Tm) %>% do(scrape_results(.))

# Scrape schedule and results for a major league team from Baseball-Reference.com

# team_abrv <- function(tm, year) {
#   teams <- html("http://www.baseball-reference.com/leagues/MLB/2015.shtml", stringsAsFactors=FALSE) %>% html_nodes("table") %>% .[[]]
#   teams<-select(teams, Tm)
#   teams<-filter(teams, Tm!="LgAvg")
# }

team_results_bref <-function(Tm, year) {
  data <- read_html(paste0("http://www.baseball-reference.com/teams/", Tm, "/", year, "-schedule-scores.shtml")) %>% html_nodes("table") %>% .[[length(.)]] %>% html_table() %>% .[,-4]
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
  data$Year <- year
  data <- data[,c(21, 1:20)]
  data
}

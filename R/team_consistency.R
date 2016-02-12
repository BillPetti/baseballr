#load packages: currently requires XML to scrape, dplyr to tidy, reldist for Gini calculations. Will be switching to rvest 

team_consistency <- function(year) {
  teams <- readHTMLTable(paste0("http://www.baseball-reference.com/leagues/MLB/", year, ".shtml"), stringsAsFactors=FALSE)
  teams <- teams[[2]]
  teams <- select(teams, Tm)
  teams <- filter(teams, Tm!="LgAvg")
  teams$year <- year
  scrape_results <- function(Tm, year) {
    url <- paste0("http://www.baseball-reference.com/teams/", Tm, "/", year, "-schedule-scores.shtml")
    data <- readHTMLTable(url, stringsAsFactors = FALSE)
    data <- data[[6]]
    data
  }
  results <- teams %>% group_by(Tm, year) %>% do(scrape_results(.$Tm, .$year))
  results  
  cols <- c(1, 4:6, 10:11)
  results <- results[,cols]
  names(results) <- c("Year", "Date", "box", "Team", "R", "RA")
  attr(results, "vars") <- NULL
  results <- filter(results, box=="boxscore")
  results$R <- as.numeric(results$R)
  results$RA <- as.numeric(results$RA)
  RGini <- aggregate(R ~ Team, data = results, FUN = "gini")
  RAGini <- aggregate(RA ~ Team, data = results, FUN = "gini")
  VOL <- left_join(RGini, RAGini, by = "Team")
  VOL$R <- round(VOL$R, 2)
  VOL$RA <- round(VOL$RA, 2)
  colnames(VOL)[1] <- "bref_t"
  VOL <- VOL %>% mutate(percrank = rank(R)/length(R))
  colnames(VOL)[4] <- "R_Ptile"
  VOL <- VOL %>% mutate(percrank = rank(RA)/length(RA))
  colnames(VOL)[5] <- "RA_Ptile"
  VOL$R_Ptile <- round(VOL$R_Ptile, 2)*100
  VOL$RA_Ptile <- round(VOL$RA_Ptile, 2)*100
  names(VOL) <- c("Team", "Con_R", "Con_RA", "Con_R_Ptile", "Con_RA_Ptile")
  VOL
}
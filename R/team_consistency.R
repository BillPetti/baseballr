#' Calculate Team-level Consistency
#'
#' This function allows you to calculate team-level consistency in run scoring and run prevention over the course of an entire season.
#' @param year Season consistency should be run for.
#' @keywords MLB, sabermetrics
#' @importFrom dplyr do_ group_by_ left_join mutate_ select_ last summarize_
#' @importFrom reldist gini
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @export
#' @examples \dontrun{team_consistency(2015)}

#load packages: currently requires XML to scrape, dplyr to tidy, reldist for Gini calculations. Will be switching to rvest for scraping

team_consistency <- function(year) {

  url <- paste0("http://www.baseball-reference.com/leagues/MLB/",year,".shtml")
  teams <- xml2::read_html(url)
  teams <- teams %>% rvest::html_nodes("table") %>%
    .[1] %>%
    html_table() %>%
    as.data.frame()
  teams <- dplyr::select_(teams, ~Tm)
  teams <- dplyr::filter_(teams, ~Tm!="LgAvg", ~Tm!="Tm")
  teams <- teams[c(1:30),] %>% as.data.frame()
  teams$year <- 2017
  names(teams) <- c("Tm", "year")

  results <- teams %>% group_by_(~Tm, ~year) %>% do_(~team_results_bref(.$Tm, .$year))
  results <- dplyr::select_(results, ~year, ~Date, ~Tm, ~R, ~RA)
  names(results) <- c("Year", "Date", "Team", "R", "RA")
  attr(results, "vars") <- NULL
  results$R <- as.numeric(results$R)
  results$RA <- as.numeric(results$RA)
  results <- dplyr::filter_(results, ~!is.na(R))
  RGini <- results %>% group_by_(~Team) %>% summarize_(R = ~gini(R))
  RAGini <- results %>% group_by_(~Team) %>% summarize_(RA = ~gini(RA))
  VOL <- left_join(RGini, RAGini, by = "Team")
  VOL$R <- round(VOL$R, 2)
  VOL$RA <- round(VOL$RA, 2)
  colnames(VOL)[1] <- "bref_t"
  VOL <- VOL %>% mutate_(percrank = ~rank(R)/length(R))
  colnames(VOL)[4] <- "R_Ptile"
  VOL <- VOL %>% mutate_(percrank = ~rank(RA)/length(RA))
  colnames(VOL)[5] <- "RA_Ptile"
  VOL$R_Ptile <- round(VOL$R_Ptile, 2)*100
  VOL$RA_Ptile <- round(VOL$RA_Ptile, 2)*100
  names(VOL) <- c("Team", "Con_R", "Con_RA", "Con_R_Ptile", "Con_RA_Ptile")

  VOL
}

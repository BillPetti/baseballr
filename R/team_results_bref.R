#' Scrape Team Results
#'
#' This function allows you to scrape schedule and results for a major league team from Baseball-Reference.com
#' @param Tm The abbreviation used by Baseball-Reference.com for the team whose results you want to scrape.
#' @param year Season for which you want to scrape the park factors.
#' @importFrom dplyr filter_ rename filter
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' team_results_bref("NYM", 2015)
#' team_results_bref("TBR", 2008)

team_results_bref <-function(Tm, year) {
  data <- read_html(paste0("http://www.baseball-reference.com/teams/", Tm, "/", year, "-schedule-scores.shtml")) %>% html_nodes("table")
  data <- data[[length(data)]] %>% html_table() %>% `[`(-3) %>%
    rename(Record = `W-L`, Result = `W/L`, Gm = `Gm#`)

  col_names <- c('Gm','Date','Tm','H_A','Opp','Result','R','RA','Inn','Record','Rank','GB','Win','Loss','Save','Time','D/N','Attendance','Streak')

  names(data) <- col_names

  data$H_A <- ifelse(grepl("@", data$H_A, fixed = TRUE), "A", "H")
  #data <- filter_(data, ~Rk != "Rk")

  data$Attendance <- gsub(",", "", data$Attendance)

  data$Streak <- ifelse(grepl("-", data$Streak, fixed = TRUE), nchar(data$Streak) * -1, nchar(data$Streak) * 1)

  for (i in c("R", "Rank", "Attendance")) {
    if(!is.numeric(data[, i])) {
      data[,i] <- suppressWarnings(as.numeric(data[,i]))
    }
  }

  data$Year <- year
  data <- data[, 1:ncol(data)]
  data <- data %>%
    filter(!grepl("Gm#", Gm))

  return(data)
}


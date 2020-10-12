#' Scrape Team Results
#'
#' This function allows you to scrape schedule and results for a major league team from Baseball-Reference.com
#' @param Tm The abbreviation used by Baseball-Reference.com for the team whose results you want to scrape.
#' @param year Season for which you want to scrape the park factors.
#' @import dplyr
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' team_results_bref("NYM", 2015)
#' team_results_bref("TBR", 2008)

team_results_bref <-function(Tm, year) {

  message('Data courtesy of Baseball-Reference.com. Please consider supporting Baseball-Reference by signing up for a Stathead account: https://stathead.com')

  url <- paste0("https://www.baseball-reference.com/teams/", Tm, "/", year, "-schedule-scores.shtml")

  data <- read_html(url) %>%
    html_nodes("table")

  data <- data[[length(data)]] %>%
    html_table() %>%
    .[-3]

  col_names <- c('Gm','Date','Tm','H_A','Opp','Result','R','RA','Inn','Record','Rank',
                 'GB','Win','Loss','Save','Time','D/N','Attendance', 'cLI', 'Streak', 'Orig_Scheduled')

  names(data) <- col_names

  data$H_A <- ifelse(grepl("@", data$H_A, fixed = TRUE), "A", "H")

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
    dplyr::filter(!grepl("Gm#", Gm))

  return(data)
}


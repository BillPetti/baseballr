#' @rdname bref_team_results
#' @title **Scrape Team Results**
#'
#' @description This function allows you to scrape schedule and results for a major league team from Baseball-Reference.com
#' @param Tm The abbreviation used by Baseball-Reference.com for the team whose results you want to scrape.
#' @param year Season for which you want to scrape the park factors.
#' @return Returns a tibble of MLB team results and the following columns:
#' 
#'  |col_name       |types     |
#'  |:--------------|:---------|
#'  |Gm             |numeric   |
#'  |Date           |character |
#'  |Tm             |character |
#'  |H_A            |character |
#'  |Opp            |character |
#'  |Result         |character |
#'  |R              |numeric   |
#'  |RA             |numeric   |
#'  |Inn            |character |
#'  |Record         |character |
#'  |Rank           |numeric   |
#'  |GB             |character |
#'  |Win            |character |
#'  |Loss           |character |
#'  |Save           |character |
#'  |Time           |character |
#'  |D/N            |character |
#'  |Attendance     |numeric   |
#'  |cLI            |numeric   |
#'  |Streak         |numeric   |
#'  |Orig_Scheduled |character |
#'  |Year           |numeric   |
#'  
#' @importFrom dplyr filter select mutate_at
#' @import rvest 
#' @export
#' @examples
#' \donttest{
#'   try(bref_team_results("NYM", 2015))
#'   try(bref_team_results(Tm="TBR", year=2008))
#' }

bref_team_results <-function(Tm, year) {
  
  if(nchar(Tm)>3){
    teams_data <- baseballr::teams_lu_table
    Tm <- (teams_data %>% 
             dplyr::filter(.data$name == Tm) %>% 
             dplyr::select(.data$bref_abbreviation))[[1]]
  }
  url <- paste0("https://www.baseball-reference.com/teams/", Tm, "/", year, "-schedule-scores.shtml")
  
  tryCatch(
    expr = {
      data <- url %>% 
        xml2::read_html() %>%
        rvest::html_elements("table")
      
      data <- data[[length(data)]] %>%
        rvest::html_table() 
      data <- data[-3]
      
      col_names <- c('Gm','Date','Tm','H_A','Opp','Result','R','RA','Inn','Record','Rank',
                     'GB','Win','Loss','Save','Time','D/N','Attendance', 'cLI', 'Streak', 'Orig_Scheduled')
      
      names(data) <- col_names
      
      data$H_A <- ifelse(grepl("@", data$H_A, fixed = TRUE), "A", "H")
      
      data$Attendance <- gsub(",", "", data$Attendance)
      
      data$Streak <- ifelse(grepl("-", data$Streak, fixed = TRUE), nchar(data$Streak) * -1, nchar(data$Streak) * 1)
      suppressWarnings(
        data <- data %>%
          dplyr::filter(!is.na(as.numeric(.data$R))) %>%
          dplyr::mutate_at(c("Gm","R","RA", "Rank", "Attendance","cLI"), as.numeric) 
      )
      data$Year <- year
      data <- data[, 1:ncol(data)]
      data <- data %>%
        dplyr::filter(!grepl("Gm#", .data$Gm))
      
      data <- data %>%
        make_baseballr_data("MLB Team Results data from baseball-reference.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team results data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(data)
}
#' @rdname team_results_bref
#' @title **(legacy) Scrape Team Results**
#' @inheritParams bref_team_results
#' @return Returns a tibble of MLB team results
#' @keywords legacy
#' @export
team_results_bref <- bref_team_results

#' @rdname sptrc_league_payrolls
#' @title **Scrape League Payroll Breakdowns from Spotrac**
#' @description This function allows you to scrape each team's payroll from Spotrac.
#' @param year Year to load
#' @return A data frame of contract data.
#'  |col_name             |types     |
#'  |:--------------------|:---------|
#'  |year                 |character |
#'  |team                 |character |
#'  |team_abbr            |character |
#'  |rank                 |numeric   |
#'  |win_percent          |numeric   |
#'  |roster               |numeric   |
#'  |active_man_payroll   |numeric   |
#'  |injured_reserve      |numeric   |
#'  |retained             |numeric   |
#'  |buried               |numeric   |
#'  |suspended            |numeric   |
#'  |yearly_total_payroll |numeric   |
#' @import rvest 
#' @import dplyr
#' @importFrom janitor clean_names
#' @export
#' @examples \donttest{
#'   try(sptrc_league_payrolls(year = most_recent_mlb_season()))
#' }
sptrc_league_payrolls <- function(year = most_recent_mlb_season()){
  
  stopifnot("'year' can't be further than two seasons ago" = 2 >= most_recent_mlb_season()-year)
  
  url <- paste0("https://www.spotrac.com/mlb/payroll/", year, "/")
  
  
  tryCatch(
    expr = {
      page_data <- rvest::read_html(url) %>% rvest::html_elements("table")
      
      league_payroll <- (page_data)[[1]] %>% 
        rvest::html_table() %>% 
        janitor::clean_names() %>% 
        dplyr::filter(.data$team != "League Average") %>%
        dplyr::rename(active_man_payroll = 5,
                      yearly_total_payroll = 10) %>%  
        dplyr::mutate(year = year,
                      team_abbr = gsub(".*\\t","", .data$team),
                      team = gsub("\\n.*","", .data$team),
                      dplyr::across(tidyr::everything(), as.character)) %>% 
        dplyr::select(.data$year, .data$team, .data$team_abbr, .data$rank, tidyr::everything())
      
      
      league_payroll[] <- lapply(league_payroll, gsub, pattern="\\$", replacement="")
      league_payroll[] <- lapply(league_payroll, gsub, pattern=",", replacement="")
      league_payroll[] <- lapply(league_payroll, gsub, pattern="-", replacement="")
      
      for(i in c(4:ncol(league_payroll))) {
        suppressWarnings(
          league_payroll[,i] <- as.numeric(unlist(league_payroll[,i]))
        )
      }
      
      league_payroll <- league_payroll %>%
        make_baseballr_data("MLB Payroll data from Spotrac.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no contract data available!"))
    },
    finally = {
    }
  )
  
  return(league_payroll)
  
}

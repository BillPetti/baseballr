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
  
  # Spotrac moved the year out of the path and behind a "/_/year/" segment in
  # 2025; the old "/payroll/<year>/" URL now 302-redirects here (#392).
  url <- paste0("https://www.spotrac.com/mlb/payroll/_/year/", year, "/")

  # Initialise the return value before the tryCatch so a failed request returns
  # an empty frame with a message instead of "object 'league_payroll' not found".
  league_payroll <- data.frame()

  tryCatch(
    expr = {
      page_data <- rvest::read_html(url) |> rvest::html_elements("table")
      
      league_payroll <- (page_data)[[1]] |> 
        rvest::html_table() |> 
        janitor::clean_names() |> 
        dplyr::filter(.data$team != "League Average") |>
        dplyr::mutate(year = year,
                      team_abbr = sub("(?s)[\\n\\t].*", "", .data$team, perl = TRUE),
                      team = trimws(sub("(?s).*[\\t\\n]\\s*", "", .data$team, perl = TRUE)),
                      dplyr::across(tidyr::everything(), as.character)) |>
        dplyr::select(
          "year", 
          "team", 
          "team_abbr", 
          "rank", 
          tidyr::everything())
      
      
      league_payroll[] <- lapply(league_payroll, gsub, pattern="\\$", replacement="")
      league_payroll[] <- lapply(league_payroll, gsub, pattern=",", replacement="")
      league_payroll[] <- lapply(league_payroll, gsub, pattern="-", replacement="")
      
      for(i in c(4:ncol(league_payroll))) {
        suppressWarnings(
          league_payroll[,i] <- as.numeric(unlist(league_payroll[,i]))
        )
      }
      
      league_payroll <- league_payroll |>
        make_baseballr_data("MLB Payroll data from Spotrac.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no contract data available!")
    },
    finally = {
    }
  )
  
  return(league_payroll)
  
}

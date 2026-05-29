#' @rdname sptrc_team_active_payroll
#' @title **Scrape Team Active Payroll Breakdown from Spotrac**
#' @description This function allows you to scrape a team's active payroll from Spotrac.
#' @param team_abbr Team abbreviation
#' @param year Year to load
#' @return A data frame of contract data.
#' 
#'  |col_name           |types     |
#'  |:------------------|:---------|
#'  |year               |numeric   |
#'  |team               |character |
#'  |player_name        |character |
#'  |roster_status      |character |
#'  |age                |numeric   |
#'  |pos                |numeric   |
#'  |status             |numeric   |
#'  |waiver_options     |numeric   |
#'  |base_salary        |numeric   |
#'  |signing_bonus      |numeric   |
#'  |payroll_salary     |numeric   |
#'  |adj_salary         |numeric   |
#'  |payroll_percent    |numeric   |
#'  |lux_tax_salary     |numeric   |
#'  |total_salary       |numeric   |
#'  
#' @import rvest 
#' @import dplyr
#' @importFrom janitor clean_names
#' @export
#' @examples \donttest{
#'  try(sptrc_team_active_payroll(team_abbr = "BAL", year = most_recent_mlb_season()))
#' }
sptrc_team_active_payroll <- function(team_abbr, year = most_recent_mlb_season()){
  
  stopifnot("'year' can't be further than two seasons ago" = 2 >= as.integer(most_recent_mlb_season())-as.integer(year))
  
  url_team_name <- switch(team_abbr,
                          "ARI" = "arizona-diamondbacks",
                          "ATL" = "atlanta-braves",
                          "BAL" = "baltimore-orioles",
                          "BOS" = "boston-red-sox",
                          "CHC" = "chicago-cubs",
                          "CWS" = "chicago-white-sox",
                          "CHW" = "chicago-white-sox",
                          "CIN" = "cincinnati-reds",
                          "CLE" = "cleveland-guardians",
                          "COL" = "colorado-rockies",
                          "DET" = "detroit-tigers",
                          "HOU" = "houston-astros",
                          "KC"  = "kansas-city-royals",
                          "KCR" = "kansas-city-royals",
                          "LAA" = "los-angeles-angels",
                          "LAD" = "los-angeles-dodgers",
                          "MIA" = "miami-marlins",
                          "MIL" = "milwaukee-brewers",
                          "MIN" = "minnesota-twins",
                          "NYM" = "new-york-mets",
                          "NYY" = "new-york-yankees",
                          "OAK" = "oakland-athletics",
                          "PHI" = "philadelphia-phillies",
                          "PIT" = "pittsburgh-pirates",
                          "SD"  = "san-diego-padres",
                          "SDP" = "san-diego-padres",
                          "SF"  = "san-francisco-giants",
                          "SFG" = "san-francisco-giants",
                          "SEA" = "seattle-mariners",
                          "STL" = "st-louis-cardinals",
                          "TB"  = "tampa-bay-rays",
                          "TBR" = "tampa-bay-rays",
                          "TEX" = "texas-rangers",
                          "TOR" = "toronto-blue-jays",
                          "WSH" = "washington-nationals",
                          "WAS" = "washington-nationals",
                          "WSN" = "washington-nationals",
                          NA)
  
  # Spotrac moved the year out of the path and behind a "/_/year/" segment in
  # 2025; the old "/payroll/<year>/" URL now 302-redirects here (#392).
  url <- paste0("https://www.spotrac.com/mlb/", url_team_name, "/payroll/_/year/", year, "/")

  # Initialise the return value before the tryCatch so a failed request returns
  # an empty frame with a message instead of "object 'Active_Payroll' not found".
  Active_Payroll <- data.frame()

  tryCatch(
    expr = {
      page_data <- rvest::read_html(url) |> rvest::html_elements("table")
      
      Active <- (page_data)[[1]] |> 
        rvest::html_table() |> 
        janitor::clean_names() |> 
        dplyr::rename("player_name" = 1) |>  
        dplyr::mutate(year = year, 
                      team = team_abbr, 
                      roster_status = "Active",
                      player_name = trimws(sub("(?s).*[\\t\\n]\\s*", "", .data$player_name, perl = TRUE)),
                      dplyr::across(tidyr::everything(), as.character)) |> 
        dplyr::select(
          "year", 
          "team", 
          "player_name", 
          "roster_status", 
          tidyr::everything())
      
      
      IL <- (page_data)[[2]] |> 
        rvest::html_table() |> 
        janitor::clean_names() |> 
        dplyr::rename("player_name" = 1) |> 
        dplyr::mutate(year = year, 
                      team = team_abbr, 
                      roster_status = "IL",
                      player_name = trimws(sub("(?s).*[\\t\\n]\\s*", "", .data$player_name, perl = TRUE)),
                      dplyr::across(tidyr::everything(), as.character)) |> 
        dplyr::select(
          "year", 
          "team", 
          "player_name", 
          "roster_status", 
          tidyr::everything())
      
      
      Retained <- (page_data)[[3]] |> 
        rvest::html_table() |> 
        janitor::clean_names() |> 
        dplyr::rename("player_name" = 1) |> 
        dplyr::mutate(year = year, 
                      team = team_abbr, 
                      roster_status = "Retained Salary",
                      player_name = trimws(sub("(?s).*[\\t\\n]\\s*", "", .data$player_name, perl = TRUE)),
                      status = NA_character_,
                      waiver_options = NA_character_,
                      dplyr::across(tidyr::everything(), as.character)) |> 
        dplyr::select(
          "year",
          "team",
          "player_name",
          "roster_status",
          tidyr::everything())
      
      Active_Payroll <- dplyr::bind_rows(Active, IL, Retained) |> 
        dplyr::mutate(signing_bonus = dplyr::if_else(.data$signing_bonus == "-", NA_character_, .data$signing_bonus))
      
      
      Active_Payroll[] <- lapply(Active_Payroll, gsub, pattern="\\$", replacement="")
      Active_Payroll[] <- lapply(Active_Payroll, gsub, pattern=",", replacement="")

      # Coerce the numeric columns by name rather than position so a change in
      # Spotrac's column order can't silently NA-coerce a text column (#392).
      num_cols <- c("year", "age",
                    grep("salary|bonus|payroll|tax|percent",
                         names(Active_Payroll), value = TRUE, ignore.case = TRUE))
      Active_Payroll <- Active_Payroll |>
        dplyr::mutate(dplyr::across(dplyr::any_of(num_cols),
                                    ~ suppressWarnings(as.numeric(.x))))
      
      Active_Payroll <- Active_Payroll |>
        make_baseballr_data("MLB Active Payroll data from Spotrac.com",Sys.time())
      
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no contract data available!")
    },
    finally = {
    }
  )
  
  return(Active_Payroll)
  
}

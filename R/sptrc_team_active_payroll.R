#' @rdname sptrc_team_active_payroll
#' @title **Scrape Team Active Payroll Breakdown from Spotrac**
#' @description This function allows you to scrape each team's active payroll from Spotrac.
#' @param team_abbr Team abbreviation
#' @return A data frame of contract data.
#'  |col_name           |types     |
#'  |:------------------|:---------|
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
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(sptrc_team_active_payroll(team_abbr = "BAL"))
#' }
sptrc_team_active_payroll <- function(team_abbr){
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
  
  url <- paste0("https://www.spotrac.com/mlb/", url_team_name,"//payroll")
  
  page_data <- rvest::read_html(url) %>% rvest::html_elements("table")
  
  Active <- (page_data)[[1]] %>% 
    rvest::html_table() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(player_name = 1) %>%  
    dplyr::mutate(team = team_abbr, 
                  roster_status = "Active",
                  player_name = gsub(".*\\t","", .data$player_name),
                  dplyr::across(tidyr::everything(), as.character)) %>% 
    dplyr::select(.data$team, .data$player_name, .data$roster_status, tidyr::everything())
  
  
  IL <- (page_data)[[2]] %>% 
    rvest::html_table() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(player_name = 1) %>% 
    dplyr::mutate(team = team_abbr, 
                  roster_status = "IL",
                  player_name = sub("[a-zA-Z'\\.\\s]*\\s{4}", "", .data$player_name),
                  player_name = sub("\\s{2}.*", "", .data$player_name),
                  dplyr::across(tidyr::everything(), as.character)) %>% 
    dplyr::select(.data$team, .data$player_name, .data$roster_status, tidyr::everything())
  
  
  Retained <- (page_data)[[3]] %>% 
    rvest::html_table() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(player_name = 1) %>% 
    dplyr::mutate(team = team_abbr, 
                  roster_status = "Retained Salary",
                  player_name = sub("[a-zA-Z]*\\s{4}", "", .data$player_name),
                  status = NA_character_,
                  waiver_options = NA_character_,
                  dplyr::across(tidyr::everything(), as.character)) %>% 
    select(.data$team, .data$player_name, .data$roster_status, .data$age, .data$pos, .data$status, .data$waiver_options, tidyr::everything())
  
  Active_Payroll <- dplyr::bind_rows(Active, IL, Retained) %>% 
    dplyr::mutate(signing_bonus = dplyr::if_else(.data$signing_bonus == "-", NA_character_, .data$signing_bonus))
  
  
  Active_Payroll[] <- lapply(Active_Payroll, gsub, pattern="\\$", replacement="")
  Active_Payroll[] <- lapply(Active_Payroll, gsub, pattern=",", replacement="")
  
  for(i in c(4,7:ncol(Active_Payroll))) {
    suppressWarnings(
      Active_Payroll[,i] <- as.numeric(unlist(Active_Payroll[,i]))
    )
  }
  
  return(Active_Payroll)
}

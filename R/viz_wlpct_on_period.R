#' Scrape MLB Standings on a Given Period and Visualize the Win-Loss percentage (WLpct) on any division or league
#'
#' This function allows you to scrape the standings from MLB for a period you choose, and visualize the Win-Loss percentage of teams along that period.
#' @param start_date a date object representing the first date of the period
#' @param end_date a date object representing the last date of the period
#' @param lg_div One or more of AL East, AL Central, AL West,
#' AL Overall, NL East, NL Central, NL West, and NL Overall
#' @keywords MLB, standings
#' @importFrom highcharter hchart hc_title hc_subtitle hc_credits hc_yAxis hc_xAxis hc_add_theme hcaes hc_theme_smpl highchart hc_add_series
#' @importFrom pbapply pbsapply
#' @importFrom tidyr separate
#' @importFrom lubridate year
#' @importFrom teamcolors teamcolors
#' @importFrom Lahman Teams
#' @export viz_wlpct_on_period
#' @examples
#' \dontrun{
#' viz_wlpct_on_period("2018-03-29","2018-04-25", "AL Overall")
#' }

viz_wlpct_on_period <- function(start_date, end_date, lg_div) {
  
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "days")   # Crate a vector of dates for the period
  standings <- pbapply::pbsapply(dates, standings_on_date_bref, division = lg_div)   # Get all the standings for each date in the period
  
  all <- do.call("rbind", standings)
  all$id <- rep(names(standings), sapply(standings, nrow))
  rownames(all) <- NULL
  names(all) <- c("Team", "W", "L", "WLpct", "GB", "RS", "RA", "pythWLpct", "id")
  all <- all %>%
    tidyr::separate(id, c("League", "From", "Date"), "_")
  all <- tbl_df(all)
  all$GB[all$GB == "--"] <- 0
  all$GB <- as.numeric(all$GB, digits = 2)
  all$pythWLpct[is.na(all$pythWLpct)] <- 0
  all$Date <- as.Date(all$Date)
  all <- all %>% select(League, Date, Team, W, L, WLpct, GB)
  
  # Print standings table for "start_date" and "end_date"
  first_end <- all %>%
    filter(Date == min(Date) | Date == max(Date)) %>%
    arrange(Date, desc(WLpct))
  print(first_end)
  
  # Create a table of colors by team
  team_palette <- Lahman::Teams %>% 
    filter(yearID == 2016) %>% 
    select(name, teamIDBR) %>% 
    left_join(teamcolors::teamcolors, by = "name") %>% 
    rename(Team = teamIDBR) %>% 
    select(Team, primary, secondary)
  
  # Replace "primary" color of some teams by its "secondary" color
  team_palette[c(1, 2, 6, 11, 16, 17, 21, 24, 27, 28), 2] <- team_palette[c(1, 2, 6, 11, 16, 17, 21, 24, 27, 28), 3]
  # 1 ARI, 2 ATL, 6 CHC, 11 HOU, 16 MIL, 17 MIN, 21 PHI, 24 SEA, 27 TBR, 28 TEX
  team_palette[,3] <- NULL # Remove "secondary" column 
  
  # Join "primary" color column to all data.frame
  all <- all %>%
    dplyr::left_join(team_palette, "Team")
  
  ov <- c("AL Overall", "NL Overall")
  div <- c("AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West")
  
  # Plot Game Behind timeline
  
  wl <- highcharter::highchart()
  
  # Add series for each team in the data frame
  for (i in 1:length(unique(all$Team))) {
    wl <- wl %>% 
      highcharter::hc_add_series(data = all[all$Team == all$Team[i],],
                                 highcharter::hcaes(x = Date, y = WLpct),
                                 name = all$Team[i],
                                 color = all$primary[i],
                                 type = "line",
                                 lineWidth = 4)
  }
  
  # Customize the plot
  wl <- wl %>% 
    highcharter::hc_xAxis(title = list(text = "Date"),
                          type = "datetime") %>% # X axis definition
    highcharter::hc_yAxis(title = list(text = "W-L%"),
                          max = 1,
                          valueDecimals = 3) %>% # Y axis definition
    highcharter::hc_title(text = paste("<span style=\"color:#002d73\"> MLB - </span>",
                                       lubridate::year(all$Date[1]),
                                       all$League[1],
                                       "Win-Loss percentage")) %>%
    highcharter::hc_subtitle(text = paste("from", start_date, "to", end_date)) %>%
    highcharter::hc_credits(enabled = TRUE, # add credits
                            text = "Source: Baseball Reference. Using 'baseballr' R package") %>%
    highcharter::hc_add_theme(highcharter::hc_theme_smpl()) %>%
    highcharter::hc_tooltip(valueDecimals = 3,
                            borderWidth = 1,
                            sort = TRUE, # Future feature to show all the teams in the tooltip
                            shared = TRUE, # Future feature to show all the teams in the tooltip
                            borderColor = "#000000") %>% # round the value to the decimals
    highcharter::hc_exporting(enabled = TRUE) # enable exporting option 
  
  
  # print viz of Games Behind
  print(wl) 
}
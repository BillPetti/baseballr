#' Scrape MLB Standings on a Given Period and Visualize the Win-Loss percentage (WLpct) on any division or league
#'
#' This function allows you to scrape the standings from MLB for a period you choose, and visualize the Win-Loss percentage of teams along that period.
#' @param start_date a date object representing the first date of the period
#' @param end_date a date object representing the last date of the period
#' @param lg_div One or more of AL East, AL Central, AL West,
#' AL Overall, NL East, NL Central, NL West and NL Overall
#' @keywords MLB, standings
#' @importFrom highcharter hchart hc_title hc_subtitle hc_credits hc_yAxis hc_xAxis hc_add_theme hcaes hc_theme_smpl highchart hc_add_series
#' @importFrom pbapply pbsapply
#' @importFrom dplyr tbl_df left_join select filter rename arrange
#' @importFrom tidyr separate
#' @importFrom lubridate year
#' @importFrom teamcolors teamcolors
#' @importFrom Lahman Teams
#' @export viz_wlpct_on_period
#' @examples
#' \dontrun{
#' viz_wlpct_on_period("2018-03-29","2018-04-29", "AL Overall")
#' }

viz_wlpct_on_period <- function(start_date, end_date, lg_div) {
  
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "days")   # Crate a vector of dates for the period
  
  if (lg_div == "MLB") {
    
    print("Getting AL Overall Standings between dates")
    standings_al <- pbapply::pbsapply(dates, standings_on_date_bref, division = "AL Overall")
    #al <- do.call("rbind", standings_al)  # binds all the dates standings into one 
    
    print("Getting NL Overall Standings between dates")
    standings_nl <- pbapply::pbsapply(dates, standings_on_date_bref, division = "NL Overall")
    #nl <- do.call("rbind", standings_nl)  # binds all the dates standings into one 
    
    standings <- append(standings_al, standings_nl)
    
  } else {
    
    standings <- pbapply::pbsapply(dates, standings_on_date_bref, division = lg_div)
    
  }
  
  all <- do.call("rbind", standings)  # binds all the dates standings into one 
  all$id <- rep(names(standings), sapply(standings, nrow))  # creates a new variable with the corresponding date of each row
  rownames(all) <- NULL    # resets rowname
  names(all) <- c("Team", "W", "L", "WLpct", "GB", "RS", "RA", "pythWLpct", "id")   # rename variables of the data frame
  
  all <- all %>%
    tidyr::separate(id, c("League", "From", "Date"), "_") %>% # separates the "id" column into several ones
    dplyr::tbl_df()
  
  if (lg_div == "MLB") { all$League = "Overall"}
  
  # cleans up the data frame
  all$GB[all$GB == "--"] <- 0
  all$GB <- as.numeric(all$GB, digits = 2)
  all$pythWLpct[is.na(all$pythWLpct)] <- 0
  all$Date <- as.Date(all$Date)
  all <- all %>%
    dplyr::select(League, Date, Team, W, L, WLpct, GB) %>% 
    dplyr::arrange(Date, desc(WLpct))
  
  # Determine which teams are leading each Division when getting Overall data per league
  if (lg_div == "AL Overall" | lg_div == "NL Overall" | lg_div == "MLB") {
    
    if (lg_div == "AL Overall") {
    
      divisions <- c("AL East", "AL Central", "AL West")
      print(paste("Getting AL Leaders by", end_date))
      print(paste("Lines of AL Division Leaders by", end_date, "will be thicker in the plot"))

    } else if (lg_div == "NL Overall") {
      
      divisions <- c("NL East", "NL Central", "NL West")
      print(paste("Getting NL Leaders by", end_date))
      print(paste("Lines of NL Division Leaders by", end_date, "will be thicker in the plot"))
    
    } else if (lg_div == "MLB") {
      
      divisions <- c("AL East", "AL Central", "AL West",
                     "NL East", "NL Central", "NL West")
      print(paste("Getting MLB Division Leaders by", end_date))
      print(paste("Lines of MLB Division Leaders by", end_date, "will be thicker in the plot"))
      
    }
    
    # Create a vector with division leaders
    leaders <- pbapply::pbsapply(divisions, standings_on_date_bref, date = end_date)
    leaders <- do.call("rbind", leaders)
    leaders$GB[leaders$GB == "--"] <- 0
    leaders$GB <- as.numeric(leaders$GB, digits = 2)
    leaders <- leaders[leaders$GB == 0, 1]

  }
  
  # Print standings table for "start_date" and "end_date"
  first_end <- all %>%
    dplyr::filter(Date == min(Date) | Date == max(Date)) %>%
    dplyr::arrange(Date, desc(WLpct)) %>% 
    print()
  
  # Create a table of colors by team
  team_palette <- Lahman::Teams %>% 
    dplyr::filter(yearID == 2016) %>% 
    dplyr::select(name, teamIDBR) %>% 
    dplyr::left_join(teamcolors::teamcolors, by = "name") %>% 
    dplyr::rename(Team = teamIDBR) %>% 
    dplyr::select(Team, primary, secondary)
  
  # Replace "primary" color of some teams by its "secondary" color
  team_palette[c(1, 2, 6, 11, 16, 17, 21, 24, 27, 28), 2] <- team_palette[c(1, 2, 6, 11, 16, 17, 21, 24, 27, 28), 3]
  # 1 ARI, 2 ATL, 6 CHC, 11 HOU, 16 MIL, 17 MIN, 21 PHI, 24 SEA, 27 TBR, 28 TEX
  team_palette[,3] <- NULL # Remove "secondary" column 
  
  # Join "primary" color column to all data.frame
  all <- all %>%
    dplyr::left_join(team_palette, "Team")
  
  # Plot Win-Loss% timeline
  
  wl <- highcharter::highchart()
  
  # Add series for each team in the data frame
  for (i in 1:length(unique(all$Team))) {
    
    if (lg_div == "AL Overall" | lg_div == "NL Overall" | lg_div == "MLB") {
      
      if (all$Team[i] %in% leaders) {
        
        wl <- wl %>%  # Creates series for teams leading their divisions (When requesting Overall)
          highcharter::hc_add_series(data = all[all$Team == all$Team[i],],
                                     highcharter::hcaes(x = Date, y = WLpct),
                                     name = all$Team[i],
                                     color = all$primary[i],
                                     type = "line",
                                     lineWidth = 4)
      
        } else {
        
        wl <- wl %>%  # Creates series for remaining teams (When requesting Overall)
          highcharter::hc_add_series(data = all[all$Team == all$Team[i],],
                                     highcharter::hcaes(x = Date, y = WLpct),
                                     name = all$Team[i],
                                     color = all$primary[i],
                                     type = "line",
                                     dashStyle = "ShortDashDotDot",
                                     lineWidth = 2)
       }
      
    } else {
      
      wl <- wl %>% # Creates plot for requested division
        highcharter::hc_add_series(data = all[all$Team == all$Team[i],],
                                   highcharter::hcaes(x = Date, y = WLpct),
                                   name = all$Team[i],
                                   color = all$primary[i],
                                   type = "line",
                                   lineWidth = 3)
    }
    
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
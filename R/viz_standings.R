#' Scrape MLB Standings on a given period and visualize timelines of Games Behind (GB) and Winning % on any division or league
#'
#' This function allows you to scrape the standings from MLB for a period you choose, and visualize GB & W% of teams along that period.
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
#' @export viz_standings_on_period
#' @examples
#' \dontrun{
#' viz_standings_on_period("2019-03-28", "2019-04-25", "AL East")
#' }

viz_standings <- function(start_date, end_date, lg_div) {
  
  # Create a vector of dates for the period
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "days")
  
  if (lg_div == "MLB") { # Get Standings for all MLB
    
    print("Getting AL Overall Standings on given period ...")
    standings_al <- pbapply::pbsapply(dates, standings_on_date_bref, division = "AL Overall")
    
    print("Getting NL Overall Standings on given period ...")
    standings_nl <- pbapply::pbsapply(dates, standings_on_date_bref, division = "NL Overall")
    
    # Append both leagues lists into one
    standings <- append(standings_al, standings_nl)
    
  } else {  # Get Standings for a defined division our league
  
    standings <- pbapply::pbsapply(dates, standings_on_date_bref, division = lg_div)
  
  }
  
  all <- do.call("rbind", standings)  # binds all the standings into one 
  all$id <- rep(names(standings), sapply(standings, nrow))  # creates a new variable with the corresponding date of each row
  rownames(all) <- NULL    # resets rowname
  names(all) <- c("Team", "W", "L", "WLpct", "GB", "RS", "RA", "pythWLpct", "id")   # rename variables of the data frame
  
  all <- all %>%
    tidyr::separate(id, c("League", "From", "Date"), "_") %>% # separates the "id" column into several ones
    dplyr::tbl_df()
  
  if (lg_div == "MLB") { all$League = "Overall"}
  
  # cCeans up the data frame
  all$GB[all$GB == "--"] <- 0   # set 0 games behind when a team is a leader of the division/league
  all$GB <- as.numeric(all$GB, digits = 2)  # defines number of decimals for GB
  all$pythWLpct[is.na(all$pythWLpct)] <- 0  # set 0 for all pythagorean pctg not available in the table
  all$Date <- as.Date(all$Date)   # set Date column as Date type
  all <- all %>%
    dplyr::select(League, Date, Team, W, L, WLpct, GB) %>% 
    dplyr::arrange(Date, GB)
  
  # Determine which teams are leading each Division when getting Overall data per league
  if (lg_div == "AL Overall" | lg_div == "NL Overall" | lg_div == "MLB") {
    
    if (lg_div == "AL Overall") {
      
      divisions <- c("AL East", "AL Central", "AL West")
      print(paste("Getting AL Leaders by", end_date, "..."))
      print(paste("Lines of AL Division Leaders by", end_date, "will be thicker in the plot"))
      
    } else if (lg_div == "NL Overall") {
      
      divisions <- c("NL East", "NL Central", "NL West")
      print(paste("Getting NL Leaders by", end_date, "..."))
      print(paste("Lines of NL Division Leaders by", end_date, "will be thicker in the plot"))
      
    } else if (lg_div == "MLB") {
      
      divisions <- c("AL East", "AL Central", "AL West",
                     "NL East", "NL Central", "NL West")
      print(paste("Getting MLB Division Leaders by", end_date, "..."))
      print(paste("Lines of MLB Division Leaders by", end_date, "will be thicker in the plot"))
      
    }
    
    # Create a vector with division leaders on the last date of the period
    leaders <- pbapply::pbsapply(divisions, standings_on_date_bref, date = end_date)
    leaders <- do.call("rbind", leaders)
    leaders$GB[leaders$GB == "--"] <- 0
    leaders$GB <- as.numeric(leaders$GB, digits = 2)
    leaders <- leaders[leaders$GB == 0, 1]
    
  }
  
  # Print standings table for "start_date" and "end_date" in the console
  first_end <- all %>%
    dplyr::filter(Date == min(Date) | Date == max(Date)) %>%
    dplyr::arrange(Date, GB) %>% 
    print()
  
  # Create a table of colors by team
  team_palette <- Lahman::Teams %>% 
    dplyr::filter(yearID == 2016) %>% 
    dplyr::select(name, teamIDBR)
  
  team_palette$name[team_palette$teamIDBR == "LAA"] <- "Los Angeles Angels"
  
  team_palette <- team_palette %>% 
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
  
  # Save the dataframe for new queries within the same period
  #write_csv(all, "")
  
  # Plot GB timeline
  
  gb <- highcharter::highchart()
  
  # Add series of GB for each team in the data frame
  for (i in 1:length(unique(all$Team))) {
    
    if (lg_div == "AL Overall" | lg_div == "NL Overall" | lg_div == "MLB") {
      
      if (all$Team[i] %in% leaders) {
        
        gb <- gb %>%  # Creates series for teams leading their divisions (When requesting Overall)
          highcharter::hc_add_series(data = all[all$Team == all$Team[i],],
                                     highcharter::hcaes(x = Date, y = GB),
                                     name = all$Team[i],
                                     color = all$primary[i],
                                     type = "line",
                                     lineWidth = 4)
        
      } else {
        
        gb <- gb %>%  # Creates series for remaining teams (When requesting Overall)
          highcharter::hc_add_series(data = all[all$Team == all$Team[i],],
                                     highcharter::hcaes(x = Date, y = GB),
                                     name = all$Team[i],
                                     color = all$primary[i],
                                     type = "line",
                                     dashStyle = "ShortDashDotDot",
                                     lineWidth = 2)
      }
      
    } else {
      
      gb <- gb %>% # Creates plot for requested division
        highcharter::hc_add_series(data = all[all$Team == all$Team[i],],
                                   highcharter::hcaes(x = Date, y = GB),
                                   name = all$Team[i],
                                   color = all$primary[i],
                                   type = "line",
                                   lineWidth = 3)
    }
    
  }
  
  # Customize the plot
  gb <- gb %>% 
    highcharter::hc_xAxis(title = list(text = "Date"),
                          type = "datetime") %>% # X axis definition
    highcharter::hc_yAxis(title = list(text = "GB"),
                          reversed = TRUE) %>% # Y axis definition
    highcharter::hc_title(text = paste("<span style=\"color:#002d73\"> MLB - </span>",
                                       lubridate::year(all$Date[1]),
                                       all$League[1],
                                       "Standings (Games Behind)")) %>%
    highcharter::hc_subtitle(text = paste("from", start_date, "to", end_date)) %>%
    highcharter::hc_credits(enabled = TRUE, # add credits
                            text = "Source: Baseball Reference. Using 'baseballr' R package") %>%
    highcharter::hc_add_theme(highcharter::hc_theme_smpl()) %>%
    highcharter::hc_tooltip(valueDecimals = 1,
                            borderWidth = 1,
                            table = TRUE,
                            valueSuffix = ' GB',
                            sort = FALSE, # Future feature to show all the teams in the tooltip
                            shared = TRUE, # Future feature to show all the teams in the tooltip
                            borderColor = "#000000") %>% # color of tooltip border
    highcharter::hc_exporting(enabled = TRUE) # enable exporting option 
  

  #### Plot W% timeline
  
  wl <- highcharter::highchart()
  
  # Add series of GB for each team in the data frame
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
  
  # Customize the W% plot
  wl <- wl %>% 
    highcharter::hc_xAxis(title = list(text = "Date"),
                          type = "datetime") %>% # X axis definition
    highcharter::hc_yAxis(title = list(text = "W-L%"),
                         max = 1,
                         valueDecimals = 3) %>% # Y axis definition
    highcharter::hc_title(text = paste("<span style=\"color:#002d73\"> MLB - </span>",
                                       lubridate::year(all$Date[1]),
                                       all$League[1],
                                       "Winning percentage")) %>%
    highcharter::hc_subtitle(text = paste("from", start_date, "to", end_date)) %>%
    highcharter::hc_credits(enabled = TRUE, # add credits
                            text = "Source: Baseball Reference. Using 'baseballr' R package") %>%
    highcharter::hc_add_theme(highcharter::hc_theme_smpl()) %>%
    highcharter::hc_tooltip(valueDecimals = 3,
                            borderWidth = 1,
                            table = TRUE, # adds color to team names in the tooltip
                            sort = TRUE, # Future feature to show all the teams in the tooltip
                            shared = TRUE, # Future feature to show all the teams in the tooltip
                            borderColor = "#000000") %>% # tooltip border color 
    highcharter::hc_exporting(enabled = TRUE) # enable exporting option 
  
  # print viz of Games Behind
  print(gb)
  
  # print viz of Winning percentage
  print(wl)
}
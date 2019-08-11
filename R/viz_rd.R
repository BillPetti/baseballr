library(baseballr)
library(tidyverse)
#library(pbapply)
library(teamcolors)
#library(lubridate)
library(highcharter)

viz_rd <- function(Tm, year) {
  
  # lg_div possible values: "MLB", AL", "NL", "AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West"  
  
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
  
  rd <- team_results_bref(Tm, year)
  rd$Gm <- as.numeric(rd$Gm)
  rd$RA <- as.numeric(rd$RA)
  
  rd <- rd %>% 
    tidyr::separate(Date, c("wd", "Date"), sep = ", ") %>% 
    tidyr::unite(Date, c("Date", "Year"), sep = ", ") %>%
    dplyr::select(Gm, Date, Tm, Opp, R, RA) %>%
    dplyr::mutate(R_Diff = R - RA,
                  cum_R_Diff = cumsum(R_Diff)) %>% 
    dplyr::filter(complete.cases(rd))
  
  names(rd)[c(1,3)] <- c("Game", "Team") 
  
  rd <- rd %>%
    dplyr::left_join(team_palette, "Team")
  
  
  ########## GGPLOT
  # viz_gg <- ggplot(data = rd,
  #                  aes(x = Game, y = cum_R_Diff)) +
  #   geom_area(fill = team_palette$primary[team_palette$Team == Tm]) +
  #   theme_classic() + 
  #   labs(title = paste(Tm, "- Runs Differential"),
  #        subtitle = paste(year, "Season"),
  #        caption = "Source: Baseball Reference. Using 'baseballr' R package",
  #        tag = "MLB") +
  #   xlab("Games") +
  #   ylab("Runs Differential")
  # 
  # print(viz_gg)
  
  ######### HIGHCHARTER
  
  viz_hc <- highcharter::highchart()
  
  viz_hc <- viz_hc %>%
    highcharter::hc_add_series(data = rd,
                               highcharter::hcaes(x = Game,
                                                  y = cum_R_Diff),
                               type = "areaspline",
                               marker = list(enabled = FALSE),
                               color = rd$primary,
                               fillColor = "#D4DFD0",
                               negativeFillColor = "#FF988C",
                               fillOpacity = 0.1) %>%
    highcharter::hc_legend(enabled = FALSE) %>% 
    highcharter::hc_tooltip(useHTML = TRUE,
                            headerFormat = "",
                            pointFormat = "Team: {point.Team} <br>
                            Date: {point.Date} <br>
                            Game: {point.Game} <br>
                            Run Diff: {point.cum_R_Diff}",
                            borderWidth = 1,
                            borderColor = "#000000")
  
  
  viz_hc <- viz_hc %>%  
    highcharter::hc_xAxis(title = list(text = "Games")) %>% # X axis definition
    highcharter::hc_yAxis(title = list(text = "Runs Differential")) %>% # Y axis definition
    highcharter::hc_title(text = paste(rd$Team[1],
                                       "<span style=\"color:#002d73\"> - Runs Differential </span>")) %>%
    highcharter::hc_subtitle(text = paste("Through ", year, " Season")) %>%
    highcharter::hc_credits(enabled = TRUE, # add credits
                            text = "Source: Baseball Reference. Using 'baseballr' R package") %>%
    highcharter::hc_add_theme(highcharter::hc_theme_smpl())
  
  print(viz_hc)
  
  
  ########
  
  opp_palette <- team_palette
  names(opp_palette)[1] <- c("Opp")
  
  rd_opp <- rd %>% 
    dplyr::group_by(Opp) %>% 
    dplyr::summarise(R = sum(R), RA = sum(RA)) %>% 
    dplyr::mutate(R_Diff = R - RA) %>% 
    dplyr::left_join(opp_palette, "Opp") %>% 
    dplyr::arrange(desc(R_Diff), desc(R)) %>% 
    dplyr::mutate(Opp = factor(Opp, unique(Opp)))
  
  max_tick <- round(max(rd_opp$R, rd_opp$RA)*1.1)
  
  viz_opp <- highcharter::highchart()
  
  viz_opp <- viz_opp %>% 
    highcharter::hc_add_series(data = rd_opp,
                               type = "columnrange",
                               highcharter::hcaes(x = Opp, # A factor variable
                                                  low = -RA, # A numeric variable
                                                  high = R,
                                                  color = primary),
                               name = "Runs Allowed - Scored") %>% # A numeric variable
    hc_yAxis(tickPositions = c(-max_tick, 0, max_tick),
             gridLineColor = "#9ca0a1",
             title = list(text = "Runs")) %>% 
    hc_xAxis(gridLineColor = "#ffffff",
             labels = rd_opp$Opp,
             title = list(text = "Opponents")) %>% 
    highcharter::hc_legend(enabled = FALSE)
    
  
  viz_opp <- viz_opp %>% 
    highcharter::hc_add_series(data = rd_opp,
                               type = "line",
                               highcharter::hcaes(x = Opp,
                                                  y = R_Diff),
                               color = "#000000",
                               name = "R Diff vs Opp")

  viz_opp <- viz_opp %>% 
    highcharter::hc_title(text = paste(rd$Team[1],
                                       "<span style=\"color:#002d73\"> - Runs Differential vs Opponents </span>")) %>%
    highcharter::hc_subtitle(text = paste("Through ", year, " Season")) %>%
    highcharter::hc_credits(enabled = TRUE, # add credits
                            text = "Source: Baseball Reference. Using 'baseballr' R package") %>%
    highcharter::hc_add_theme(highcharter::hc_theme_smpl())
  
  
    viz_opp
}

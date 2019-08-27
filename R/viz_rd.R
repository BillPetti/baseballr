#' Visualize MLB team's Runs Differential
#' 
#' @param Tm a character object representing: Team name (BBRef), or Division, or League, or entire MLB
#' @param year a numeric object representing the season to visualize
#' @return This function allows to scrape MLB teams results on a specific season and visualizes the cumulative runs differential
#' @keywords MLB, Runs, Results, Baseball, BBRef
#' @importFrom magrittr >%>
#' @importFrom pbapply pbapply pbsapply
#' @importFrom tidyr separate unite
#' @importFrom dplyr select filter group_by mutate ungroup sumarise arrange
#' @importFrom highcharter hchart hcaes hc_tooltip hc_add_theme hc_theme_smpl hc_xAxis hc_yAxis hc_title hc_subtitle hc_credits hc_exporting hw_grid
#' @importFrom purrr map
#' @importFrom htmltools browsable
#' @export viz_rd
#' @examples
#' viz_rd("AL East", 2019)
#' viz_rd("BOS", 2018)
#' viz_rd("MLB", 2000)
#' \dontrun{
#' viz_rd("Boston Red Sox", 2019)
#' }

# Function arguments notes:
# 'Tm': the team acronym (used by Baseball Reference) or Division, or League or the entire MLB
      # Other possible values for 'Tm' besides a bbref name:
          # "MLB", "AL Overall", "NL Overall"
          # "AL East", "AL Central", "AL West"
          # "NL East", "NL Central", "NL West"
# 'year' MLB Season 

viz_rd <- function(Tm, year) {
  
  ### Identify 'Tm' input type and get names of teams to visualize
  
  if (intersect(grepl("AL|NL", Tm),
                grepl("East|Central|West|Overall", Tm))) { # Division or leagues in year
    
    teams <- standings_on_date_bref(paste0(year,"-04-30"), Tm) %>% 
      as.data.frame() %>% 
      select(1) %>% 
      unlist()
    
  } else if (Tm == "MLB") { # All MLB teams in year
    
    mlb <- c("AL Overall", "NL Overall")
    
    print(paste0("Getting names of all MLB teams that played in ", year, "..."))
    teams <- pbapply::pbsapply(mlb, standings_on_date_bref, date = paste0(year,"-04-30"))
    teams <- do.call("rbind", teams) %>% 
      select(1) %>% 
      unlist()
  
  } else { # Only one team
      teams <- Tm
    }
  
  
  print(paste0("Getting game's results data for Teams that played in ", year, " ..."))

  rd <- pblapply(teams, team_results_bref, year)      # Gets the team's results tables for each team
  rd <- do.call("rbind", rd)                          # Binds tables for all teams into one data frame
  
  rd$Gm <- as.numeric(rd$Gm)                          # change Game variable to numeric
  rd$RA <- as.numeric(rd$RA)                          # change Runs Allowed variable to numeric

  ### Tidying rd data frame
  
  rd <- rd %>% 
    tidyr::separate(Date, c("wd", "Date"), sep = ", ") %>% 
    tidyr::unite(Date, c("Date", "Year"), sep = ", ") %>%
    dplyr::select(Gm, Date, Tm, Opp, R, RA) %>%
    dplyr::filter(complete.cases(rd)) %>% 
    dplyr::group_by(Tm) %>% 
    dplyr::mutate(R_Diff = R - RA,
                  cum_R_Diff = cumsum(R_Diff)) %>% 
    dplyr::ungroup()
  
  names(rd)[c(1,3)] <- c("Game", "Team") 

  ### Determine columns for grid charts based on number of teams
  
  if (length(teams) <= 5) {                         # Either only one team or one Division
    viz_col <- 1
  } else if (length(teams) == 15) {                 # One League
    viz_col <- 3
  } else if (length(teams) == 30) {                 # All MLB
    viz_col <- 5
  }
  
  ### Defining min & max for yAxis to be the same for all charts
  min_RDiff <- round(min(rd$cum_R_Diff)*1.1)
  max_RDiff <- round(max(rd$cum_R_Diff)*1.1)
  
  ### Creating an ordered vector (not factor) of teams based on cummulated Runs Differential.
  ### NOTE: This is because 'highcharter' hc_grid function plots charts in the order they are created and not based on factors (as ggplot) 
  teams_factor <- rd %>%
    dplyr::group_by(Team) %>%
    dplyr::summarise(R = sum(R), RA = sum(RA)) %>%
    dplyr::mutate(R_Diff = R - RA) %>%
    dplyr::arrange(desc(R_Diff), desc(R)) %>%
    dplyr::select(1) %>% 
    unlist()

  ###Creating charts for each team
    
  map(teams_factor, function(x) {
    
    rd[rd$Team == x,] %>% 
      highcharter::hchart(showInLegend = FALSE,
             type = "areaspline",
             highcharter::hcaes(x = Game,
                   y = cum_R_Diff),
             marker = list(enabled = FALSE),
             color = "#4B5463",
             fillColor = "#D4DFD0",
             negativeFillColor = "#FF988C",
             fillOpacity = 0.4) %>%
      highcharter::hc_tooltip(useHTML = TRUE,
                 headerFormat = "",
                 pointFormat = "Team: {point.Team} <br>
                           Date: {point.Date} <br>
                           Game: {point.Game} <br>
                           Run Diff: {point.cum_R_Diff}",
                 borderWidth = 1,
                 borderColor = "#000000") %>%
      highcharter::hc_add_theme(hc_theme_smpl()) %>% 
      highcharter::hc_xAxis(title = list(text = "Games")) %>%          # X axis definition
      highcharter::hc_yAxis(title = list(text = "R Diff"),             # Y axis definition
               min = min_RDiff,
               max = max_RDiff) %>%                       
      highcharter::hc_title(text = paste(x, "<span style=\"color:#002d73\"> - Runs Differential </span>")) %>%
      highcharter::hc_subtitle(text = paste("Through ", year, " Season")) %>%
      highcharter::hc_credits(enabled = TRUE,                          # add credits
                 text = "Source: Baseball Reference. Using 'baseballr' R package") %>% 
      highcharter::hc_exporting(enabled = TRUE) }) %>%                 # enable exporting option
    highcharter::hw_grid(rowheight = 400, ncol = viz_col)  %>%         # faceting all charts
    browsable()
      
}

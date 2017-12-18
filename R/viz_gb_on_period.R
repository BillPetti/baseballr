#' Scrape MLB Standings on a Given Period and Visualize the Games Behind (GB) on any division or league the league
#'
#' This function allows you to scrape the standings from MLB for a period you choose, and visualize the GB of teams along that period.
#' @param start_date a date object representing the first date of the period
#' @param end_date a date object representing the last date of the period
#' @param div_viz One or more of AL East, AL Central, AL West,
#' AL Overall, NL East, NL Central, NL West, and NL Overall
#' @keywords MLB, standings
#' @importFrom highcharter hchart hc_title hc_subtitle hc_credits hc_yAxis hc_xAxis hc_add_theme
#' @importFrom pbapply pbsapply
#' @export viz_gb_on_period
#' @examples
#' \dontrun{
#' viz_gb_on_period("2017-04-02","2017-04-10", "AL East")
#' }

viz_gb_on_period <- function(start_date, end_date, lg_div) {
  
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "days")   # Crate a vector of dates for the period
  standings <- pbsapply(dates, standings_on_date_bref, division = lg_div)   # 
  
  all <- do.call("rbind", standings)
  all$id <- rep(names(standings), sapply(standings, nrow))
  rownames(all) <- NULL
  names(all) <- c("Team", "W", "L", "WLpct", "GB", "RS", "RA", "pythWLpct", "id")
  all <- all %>% separate(id, c("League", "From", "Date"), "_")
  all <- tbl_df(all)
  all$GB[all$GB == "--"] <- 0
  all$GB <- as.numeric(all$GB, digits = 2)
  all$pythWLpct[is.na(all$pythWLpct)] <- 0
  all$Date <- as.Date(all$Date)
  all <- all %>% select(League, Date, Team, W, L, WLpct, GB)
  
  first_end <- all %>%
    filter(Date == min(Date) | Date == max(Date)) %>% 
    arrange(Date, GB)
  print(first_end)
  
  hchart(all, "line", hcaes(x = Date, y = GB, group = Team)) %>%
    hc_title(text = paste(all$League[1], "Standings (GB - Games behind)")) %>%
    hc_subtitle(text = paste("from", start_date, "to", end_date)) %>% 
    hc_credits(enabled = TRUE, # add credits
               text = "Source: Baseball Reference. Using 'baseballr' R package") %>% 
    hc_yAxis(title = list(text = "GB"),
             reversed = TRUE) %>%
    hc_xAxis(title = list(text = "Date")) %>% 
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_tooltip(valueDecimals = 1) %>% # round the value to the decimals
    hc_exporting(enabled = TRUE) # enable exporting option
}

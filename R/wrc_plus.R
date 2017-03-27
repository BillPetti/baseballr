#' Calculate wRC and wRC+ for any set of data
#'
#' This function allows you to calculate wRC+ for any given set of data, provided the right variables are in the data set. The function currently returns both wRC and wRC+ over a given date range.
#' NOTE: wRC and wRC+ rely on park factors and other metrics which vary on a yearly basis. If statistics from multiple years are provided, league-wide metrics will be calculated only for the first season in the provided range. 
#'
#' @param df A data frame of statistics that includes, at a minimum, the following columns: uBB (unintentional walks), HBP (Hit By Pitch), X1B (singles), X2B (doubles), X3B (triples), HR (home runs), AB (at-bats), SH (sacrafice hits), SO (strike outs), and season.
#' @keywords MLB, wrc, wrc_plus, sabermetrics
#' @export

wrc_plus <- function(df) {

  #BBRef and Fangraphs use different methods to ID teams, so use this matrix to rectify the issue.
  team_revise = c("MLB-AL Los Angeles" = "Angels",
                  "MLB-AL Baltimore" = "Orioles",
                  "MLB-AL Boston" = "Red Sox",
                  "MLB-AL Chicago" = "White Sox",
                  "MLB-AL Cleveland" = "Indians",
                  "MLB-AL Detroit" = "Tigers",
                  "MLB-AL Kansas City" = "Royals",
                  "MLB-AL Minnesota" = "Twins",
                  "MLB-AL New York" = "Yankees",
                  "MLB-AL Oakland" = "Athletics",
                  "MLB-AL Seattle" = "Mariners",
                  "MLB-AL Tampa Bay" = "Rays",
                  "MLB-AL Texas" = "Rangers",
                  "MLB-AL Toronto" = "Blue Jays",
                  "MLB-NL Arizona" = "Diamondbacks",
                  "MLB-NL Atlanta" = "Braves",
                  "MLB-NL Chicago" = "Cubs",
                  "MLB-NL Cincinnati" = "Reds",
                  "MLB-NL Colorado" = "Rockies",
                  "MLB-NL Miami" = "Marlins",
                  "MLB-NL Florida" = "Marlins",
                  "MLB-NL Houston" = "Astros",
                  "MLB-AL Houston" = "Astros",
                  "MLB-NL Los Angeles" = "Dodgers",
                  "MLB-NL Milwaukee" = "Brewers",
                  "MLB-NL Washington" = "Nationals",
                  "MLB-NL New York" = "Mets",
                  "MLB-NL Philadelphia" = "Phillies",
                  "MLB-NL Pittsburgh" = "Pirates",
                  "MLB-NL St. Louis" = "Cardinals",
                  "MLB-NL San Diego" = "Padres",
                  "MLB-NL San Francisco" = "Giants")

  df$teamfinder = paste(df$Level, df$Team)
  league <- substr(df[1, ]$Level, 5, 6)
  season <- as.numeric(df[1, ]$season)
  #Pre-2005 we encounter the Expos, which we're not yet ready to handle. Post-2015 park factors are not posted on Fangraphs as of 3/2017.
  if ((season > 2015) | (season < 2005)) {
    stop('wRC+ is only available for 2005 - 2015')
  }
  
  #Not yet sure how to handle multiple leagues or park factors in a single dataframe, so let's limit the query to a single team
  if (as.numeric(nlevels(as.factor(df$Team)) != 1)) {
    stop('wRC and wRC+ can only be calculated for one team at a time')
  }

  #scrape park factor table from Fangraphs guts
  pf_table <- read_html(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", season))
  pf_table <- pf_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
  pf_table<- as.data.frame(pf_table)[-(1:2), (1:14)]
  names(pf_table) <- c("season", "team", "pfBasic", "pf1B", "pf2B", "pf3B", "pfHR", "pfSO", "pfBB", "pfGB", "pfFB", "pfLD", "pfIFFB", "pfFIP")
  for(i in c(3:ncol(pf_table))) {
    pf_table[,i] <- as.numeric(as.character(pf_table[,i]))
  }

  #scrape FIP and wOBA constants from Fangraphs guts
  guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
  guts_table <- guts_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
  guts_table<- as.data.frame(guts_table)[-(1:2), (1:14)]
  names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  for(i in c(2:ncol(guts_table))) {
    guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
  }

  #scrape league leaderboard from fangraphs for a given season to calculate wRC/PA
  lg_table <- read_html(paste0("http://www.fangraphs.com/leaders.aspx?pos=np&stats=bat&lg=", league ,"&qual=0&type=1&season=", season ,"&month=0&season1=", season, "&ind=0&team=0,ss&rost=0&age=0&filter=&players=0"))
  lg_table <- lg_table %>%
    html_nodes(xpath = '//*[@id="LeaderBoard1_dg1"]/table') %>%
    html_table(fill = TRUE)
  lg_table <- as.data.frame(lg_table)
  lg_table <- as.data.frame(lg_table[4,-1])
  names(lg_table) = c("season", "PA", "BB%", "K%", "BB/K", "AVG", "OBP", "SLG", "OPS", "ISO", "Spd", "BABIP", "UBR", "wGDP", "wSB", "wRC", "wRAA", "wOBA", "wRC+")
  lg_table$wRCperPA <- (as.numeric(lg_table$wRC) / as.numeric(lg_table$PA))
  #rather than pull in the entire lg_table object later, let's pull out what we really want:
  wRCperPAtable <- lg_table[ , c("season", "wRCperPA")]

  #combine data from the 3 scrapes done above with user input
  df_join_first <- left_join(df, guts_table, by = "season")
  df_join_second <- left_join(df_join_first, wRCperPAtable, by = "season")
  df_join <- left_join(df_join_second, pf_table[pf_table$team == team_revise[df[1, ]$teamfinder], ], by = "season")

    #wRC requires the use of OBA, so reuse some internals of the woba_plus function
  df_join$wOBA <- round((((df_join$wBB * df_join$uBB) + (df_join$wHBP * df_join$HBP) + (df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/(df_join$AB + df_join$uBB + df_join$HBP + df_join$SF)),3)
  df_join$wOBA_CON <- round((((df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/(df_join$AB - df_join$SO)),3)

  #wRC = (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA
  df_join$wRC <- round((((((df_join$wOBA - df_join$lg_woba) / df_join$woba_scale) + (df_join$lg_r_pa)) * df_join$PA)))
  #wRC+ = (((wRAA/PA + League R/PA) + (League R/PA – Park Factor* League R/PA)) / (AL or NL wRC/PA excluding pitchers))*100
  df_join$wRC_plus <- round((((((df_join$wOBA - df_join$lg_woba) / df_join$woba_scale) + df_join$lg_r_pa) + (df_join$lg_r_pa - ((df_join$pfBasic/100) * df_join$lg_r_pa))) / df_join$wRCperPA) * 100)


  df_join <- arrange_(df_join, ~(wRC))
  x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP", "teamfinder", "wRCperPA", "team", "pfBasic", "pf1B", "pf2B", "pf3B", "pfHR", "pfSO", "pfBB", "pfGB", "pfFB", "pfLD", "pfIFFB", "pfFIP")
  df_join <- df_join[!x]
  df_join
}
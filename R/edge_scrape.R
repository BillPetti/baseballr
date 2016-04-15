#' Edge Percentage Scrape
#'
#' This function allows you to calculate the percent of pitches thrown to different edges of the strike zone over any custom time frame. Data is acquired by scraping the GameDay application from MLBAM using Carson Sievert's pitchRx package. The data can take a while to query, particular for large date ranges.
#' @param start First date in your date range. Must be a character string in the format "yyyy-mm-dd".
#' @param end Last date in your date range. Must be a character string in the format "yyyy-mm-dd".
#' @param group Character string indicating whether to group the output by pitchers or batters. Options are "pitcher" or "batter".
#' @keywords MLB, sabermetrics, PITCHf/x
#' @importFrom pitchRx scrape
#' @importFrom dplyr summarise
#' @export
#' @examples \dontrun{edge_scrape("2015-04-05", "2015-04-05", pitcher)}

edge_scrape <- function(start, end, group) {
  pfx <- scrape(start, end)
  df <- left_join(pfx$pitch, pfx$atbat, by = c("gameday_link", "num"))
  f <- as.numeric(lapply(strsplit(df$b_height, "-"), function(x) x[1])) * 12
  i <- as.numeric(lapply(strsplit(df$b_height, "-"), function(x) x[2]))
  df$b_height_inch <- f+i
  df$called_pitch <- ifelse(grepl("Called|Ball", df$des), 1, 0)
  df$called_strike <- ifelse(grepl("Called", df$des), 1, 0)
  df$swing <- ifelse(grepl("Swinging|Foul|In play", df$des), 1, 0)
  df$whiff <- ifelse(grepl("Swinging", df$des), 1, 0)
  pitcher_match <- select(df, pitcher_name, pitcher) %>% unique()
  batter_match <- select(df, batter_name, batter) %>% unique()
  LHH <- filter(df, stand == "L")
  RHH <- filter(df, stand == "R")
  LHH$location <- with(LHH, ifelse(!is.na(px) & !is.na(pz) & px > .21 & px < .81 & pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Inside Edge", ifelse(!is.na(px) & !is.na(pz) & px > -1.20 & px < -0.9 & pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Outside Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz > (1.7 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Upper Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz > (.35 + b_height_inch/12 *.229) & pz < (.65 + b_height_inch/12 *.229), "Lower Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz >= (.65 + b_height_inch/12 *.229) & pz <= (1.7 + b_height_inch/12 *.229), "Heart", ifelse(is.na(px) | is.na(pz), NA, "Out of Zone")))))))
  RHH$location <- with(RHH, ifelse(!is.na(px) & !is.na(pz) & px > -1.03 & px < -.43 & pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Inside Edge", ifelse(!is.na(px) & !is.na(pz) & px > .7 & px < 1.00 & pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Outside Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz > (2.3 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Upper Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz > (.92 + b_height_inch/12 *.136) & pz < (1.22 + b_height_inch/12 *.136), "Lower Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz >= (1.22 + b_height_inch/12 *.136) & pz <= (2.30 + b_height_inch/12 *.136), "Heart", ifelse(is.na(px) | is.na(pz), NA, "Out of Zone")))))))
  df_combined <- rbind(LHH, RHH)
  df_combined$Upper_Edge <- with(df_combined, ifelse(location == "Upper Edge", 1, 0))
  df_combined$Lower_Edge <- with(df_combined, ifelse(location == "Lower Edge", 1, 0))
  df_combined$Inside_Edge <- with(df_combined, ifelse(location == "Inside Edge", 1, 0))
  df_combined$Outside_Edge <- with(df_combined, ifelse(location == "Outside Edge", 1, 0))
  df_combined$Heart <- with(df_combined, ifelse(location == "Heart", 1, 0))
  df_combined$OutOfZone <- with(df_combined, ifelse(location == "Out of Zone", 1, 0))
  grouped <- filter(df_combined, !is.na(px), !is.na(pz)) %>% group_by_(group) %>% summarise(All_pitches = n(), All_calls = sum(called_pitch), Called_Strike = sum(called_strike), Called_strike_rate = round(sum(called_strike)/sum(called_pitch),3), Upper_Edge = sum(Upper_Edge)/All_pitches, Lower_Edge = sum(Lower_Edge)/All_pitches, Inside_Edge = sum(Inside_Edge)/All_pitches, Outside_Edge = sum(Outside_Edge)/All_pitches, Heart = sum(Heart)/All_pitches, Out_of_Zone = sum(OutOfZone)/All_pitches)
  grouped[,c(6:11)] <- round(grouped[,c(6:11)], 3)
  grouped <- if (group == "pitcher") {left_join(grouped, pitcher_match, by = "pitcher") %>% select(pitcher_name, everything())} else {left_join(grouped, batter_match, by = "batter") %>% select(batter_name, everything())}
  grouped
  }

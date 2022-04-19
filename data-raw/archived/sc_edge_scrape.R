#' @rdname statcast_edge_scrape
#' @title **Edge Percentage Scrape**
#' @description This function allows you to calculate the percent of pitches thrown to different edges of the strike zone over any custom time frame. Data is acquired by scraping the GameDay application from MLBAM using Carson Sievert's pitchRx package. The data can take a while to query, particular for large date ranges.
#' @param start First date in your date range. Must be a character string in the format "yyyy-mm-dd".
#' @param end Last date in your date range. Must be a character string in the format "yyyy-mm-dd".
#' @param group Character string indicating whether to group the output by pitchers or batters. Options are "pitcher" or "batter".
#' @return Returns a tibble with Statcast data and edge columns and calculations.
#' @export
#' @examples \donttest{
#'   try(edge_scrape(start = "2016-04-06",
#'                   end = "2016-04-15",
#'                   group = "pitchers"))
#' }
edge_scrape <- function(start, end, group) {
  pfx <- statcast_search(start, end)
  df <- left_join(pfx$pitch, pfx$atbat, by = c("game_pk", "pitch_num"))
  f <- as.numeric(lapply(strsplit(df$b_height, "-"), function(x) x[1])) * 12
  i <- as.numeric(lapply(strsplit(df$b_height, "-"), function(x) x[2]))
  df$b_height_inch <- f+i
  df$called_pitch <- ifelse(grepl("Called|Ball", df$des), 1, 0)
  df$called_strike <- ifelse(grepl("Called", df$des), 1, 0)
  df$swing <- ifelse(grepl("Swinging|Foul|In play", df$des), 1, 0)
  df$whiff <- ifelse(grepl("Swinging", df$des), 1, 0)
  pitcher_match <- df %>% 
    dplyr::select(.data$pitcher_name, .data$pitcher) %>% 
    unique()
  batter_match <- df %>% 
    dplyr::select(.data$batter_name, .data$batter) %>% 
    unique()
  LHH <- df %>% 
    dplyr::filter(.data$stand == "L")
  RHH <- df %>% 
    dplyr::filter(.data$stand == "R")
  LHH$location <- with(LHH, ifelse(!is.na(px) & !is.na(pz) & px > .21 & px < .81 & pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Inside Edge", ifelse(!is.na(px) & !is.na(pz) & px > -1.20 & px < -0.9 & pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Outside Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz > (1.7 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Upper Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz > (.35 + b_height_inch/12 *.229) & pz < (.65 + b_height_inch/12 *.229), "Lower Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz >= (.65 + b_height_inch/12 *.229) & pz <= (1.7 + b_height_inch/12 *.229), "Heart", ifelse(is.na(px) | is.na(pz), NA, "Out of Zone")))))))
  RHH$location <- with(RHH, ifelse(!is.na(px) & !is.na(pz) & px > -1.03 & px < -.43 & pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Inside Edge", ifelse(!is.na(px) & !is.na(pz) & px > .7 & px < 1.00 & pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Outside Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz > (2.3 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Upper Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz > (.92 + b_height_inch/12 *.136) & pz < (1.22 + b_height_inch/12 *.136), "Lower Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz >= (1.22 + b_height_inch/12 *.136) & pz <= (2.30 + b_height_inch/12 *.136), "Heart", ifelse(is.na(px) | is.na(pz), NA, "Out of Zone")))))))
  df_combined <- dplyr::bind_rows(LHH, RHH)
  df_combined$Upper_Edge <- with(df_combined, ifelse(location == "Upper Edge", 1, 0))
  df_combined$Lower_Edge <- with(df_combined, ifelse(location == "Lower Edge", 1, 0))
  df_combined$Inside_Edge <- with(df_combined, ifelse(location == "Inside Edge", 1, 0))
  df_combined$Outside_Edge <- with(df_combined, ifelse(location == "Outside Edge", 1, 0))
  df_combined$Heart <- with(df_combined, ifelse(location == "Heart", 1, 0))
  df_combined$OutOfZone <- with(df_combined, ifelse(location == "Out of Zone", 1, 0))
  grouped <- df_combined %>% 
    dplyr::filter(!is.na(.data$px), !is.na(.data$pz)) %>% 
    dplyr::group_by(.data$group) %>% 
    dplyr::summarise(
      All_pitches = n(), 
      All_calls = sum(.data$called_pitch), 
      Called_Strike = sum(.data$called_strike), 
      Called_strike_rate = round(sum(.data$called_strike)/sum(.data$called_pitch),3), 
      Upper_Edge = sum(.data$Upper_Edge)/.data$All_pitches, 
      Lower_Edge = sum(.data$Lower_Edge)/.data$All_pitches, 
      Inside_Edge = sum(.data$Inside_Edge)/.data$All_pitches, 
      Outside_Edge = sum(.data$Outside_Edge)/.data$All_pitches, 
      Heart = sum(.data$Heart)/.data$All_pitches, 
      Out_of_Zone = sum(.data$OutOfZone)/.data$All_pitches) %>%
    dplyr::mutate(
      Total_Edge = .data$Upper_Edge + .data$Lower_Edge + .data$Inside_Edge + .data$Outside_Edge)
  grouped[,c(6:12)] <- round(grouped[,c(6:12)], 3)
  grouped <- if (group == "pitcher") {
    grouped %>% 
      dplyr::left_join(pitcher_match, by = "pitcher") %>% 
      dplyr::select(.data$pitcher_name, tidyr::everything())
  } else {
    grouped %>% 
      dplyr::left_join(batter_match, by = "batter") %>% 
      dplyr::select(.data$batter_name, tidyr::everything())
  }
  return(grouped)
}

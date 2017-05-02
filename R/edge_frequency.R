#' Edge Percentage Frequency
#'
#' This function allows you to calculate the percent of pitches thrown to different edges of the strike zone for a pitch by pitch data set that has been coded using the edge_code() function.
#' @param df A data frame of pitch by pitch data that has been coded using the edge_code() function.
#' @param group Character string indicating what column to group the frequency by. For example, "pitcher" or "batter". Defaults to NULL, which calculates the frequencies across the entire data set.
#' @keywords MLB, sabermetrics, PITCHf/x
#' @importFrom dplyr summarise
#' @export
#' @examples \dontrun{edge_code(df, group = "pitcher")}

edge_frequency <- function(df, group = NULL) {
  if (is.null(group)) {
    grouped <- filter(df, !is.na(px), !is.na(pz)) %>%
      summarise(All_pitches = n(), All_calls = sum(called_pitch), Called_Strike = sum(called_strike), Called_strike_rate = round(sum(called_strike)/sum(called_pitch),3), Upper_Edge = sum(Upper_Edge)/All_pitches, Lower_Edge = sum(Lower_Edge)/All_pitches, Inside_Edge = sum(Inside_Edge)/All_pitches, Outside_Edge = sum(Outside_Edge)/All_pitches, Heart = sum(Heart)/All_pitches, Out_of_Zone = sum(OutOfZone)/All_pitches) %>%
      mutate(Total_Edge = Upper_Edge + Lower_Edge + Inside_Edge + Outside_Edge)
    grouped
  }
  else {
    grouped <- filter(df, !is.na(px), !is.na(pz)) %>% group_by_(group) %>% summarise(All_pitches = n(), All_calls = sum(called_pitch), Called_Strike = sum(called_strike), Called_strike_rate = round(sum(called_strike)/sum(called_pitch),3), Upper_Edge = sum(Upper_Edge)/All_pitches, Lower_Edge = sum(Lower_Edge)/All_pitches, Inside_Edge = sum(Inside_Edge)/All_pitches, Outside_Edge = sum(Outside_Edge)/All_pitches, Heart = sum(Heart)/All_pitches, Out_of_Zone = sum(OutOfZone)/All_pitches) %>%
    mutate(Total_Edge = Upper_Edge + Lower_Edge + Inside_Edge + Outside_Edge)
    grouped
  }
}

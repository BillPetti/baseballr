#' @title **Edge Percentage Frequency**
#'
#' @description This function allows you to calculate the percent of pitches thrown to different edges of the strike zone for a pitch by pitch data set that has been coded using the ```edge_code()``` function.
#' @param df A data frame of pitch by pitch data that has been coded using the ```edge_code()``` function.
#' @param group Character string indicating what column to group the frequency by. For example, "pitcher" or "batter". Defaults to NULL, which calculates the frequencies across the entire data set.
#' @return Returns a tibble with the additional edge columns necessary for frequency calculations. 
#' @export

edge_frequency <- function(df, group = NULL) {
  if (is.null(group)) {
    grouped <- df %>% 
      dplyr::filter(!is.na(.data$px), !is.na(.data$pz)) %>%
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
    return(grouped)
  }
  else {
    grouped <- df %>% 
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
    return(grouped)
  }
}

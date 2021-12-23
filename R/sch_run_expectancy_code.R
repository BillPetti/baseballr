#' Generate run expectancy and related measures and variables from Baseball Savant data
#'
#' These functions allow a user to generate run expectancy and related measures and variables from Baseball Savant data. Measures and variables will be added to the data frame.
#' @param df A data frame generated from Baseball Savant.
#' @param level Whether you want run expectancy calculated at the plate appearance or pitch level. Defaults to plate appearance.
#' @importFrom stringr str_count
#' @importFrom rlang .data
#' @export
#' @details
#' ```r
#' run_expectancy_code(df, level = "plate appearances")
#' ```

run_expectancy_code <- function(df, level = "plate appearance"){
  single_outs <- c("strikeout", "caught_stealing_2b", "pickoff_caught_stealing_2b",
                   "other_out", "caught_stealing_3b", "caught_stealing_home",
                   "field_out", "force_out", "pickoff_1b", "batter_interference",
                   "fielders_choice", "pickoff_2b", "pickoff_caught_stealing_3b",
                   "pickoff_caught_stealing_home")
  df <- df %>%
    dplyr::arrange(.data$game_pk, .data$at_bat_number, .data$pitch_number) %>%
    dplyr::group_by(.data$game_pk) %>%
    dplyr::mutate(
      final_pitch_game = ifelse(.data$pitch_number == max(.data$pitch_number), 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$game_pk, .data$at_bat_number, .data$inning_topbot) %>%
    dplyr::mutate(
      final_pitch_at_bat = ifelse(.data$pitch_number == max(.data$pitch_number), 1, 0)) %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::arrange(.data$game_pk, .data$inning_topbot, .data$at_bat_number, .data$pitch_number) %>%
    dplyr::mutate(
      runs_scored_on_pitch = stringr::str_count(.data$des, "scores"),
      runs_scored_on_pitch = ifelse(.data$events == "home_run", .data$runs_scored_on_pitch + 1, .data$runs_scored_on_pitch), 
      bat_score_after = .data$bat_score + .data$runs_scored_on_pitch) %>%
    dplyr::arrange(.data$game_pk, .data$at_bat_number,.data$ pitch_number) %>%
    dplyr::mutate(
      final_pitch_inning = ifelse(.data$final_pitch_at_bat == 1 & .data$inning_topbot != lead(.data$inning_topbot), 1, 0), 
      final_pitch_inning = ifelse(is.na(.data$final_pitch_inning), 1, .data$final_pitch_inning))
  
  if (level == "plate appearance") {
    
    df <- df %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(
        bat_score_start_inning = min(.data$bat_score),
        bat_score_end_inning = max(.data$bat_score), 
        cum_runs_in_inning = cumsum(.data$runs_scored_on_pitch), 
        runs_to_end_inning = .data$bat_score_end_inning - .data$bat_score) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        base_out_state = paste(.data$outs_when_up, " outs, ", 
                               ifelse(!is.na(.data$on_1b), "1b", "_"), 
                               ifelse(!is.na(.data$on_2b),  "2b", "_"), 
                               ifelse(!is.na(.data$on_3b), "3b", "_")))
    
    re_table <- run_expectancy_table(df)
    
    df <- df %>% left_join(re_table, by = "base_out_state")
    df <- df %>% dplyr::filter(.data$final_pitch_at_bat == 1) %>%
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(next_base_out_state = dplyr::lead(.data$base_out_state)) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(re_table, by = c("next_base_out_state" = "base_out_state")) %>%
      dplyr::rename(
        next_avg_re = .data$avg_re.y, 
        avg_re = .data$avg_re.x) %>%
      dplyr::mutate(
        next_avg_re = ifelse(is.na(.data$next_avg_re), 0, .data$next_avg_re),
        change_re = .data$next_avg_re - .data$avg_re,
        re24 = .data$change_re + .data$runs_scored_on_pitch) %>%
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot)
  }
  else {
    df <- df %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(
        bat_score_start_inning = min(.data$bat_score),
        bat_score_end_inning = max(.data$bat_score),
        cum_runs_in_inning = cumsum(.data$runs_scored_on_pitch),
        runs_to_end_inning = .data$bat_score_end_inning - .data$bat_score) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        count_base_out_state = paste(.data$balls, "-", .data$strikes, ", ", .data$outs_when_up, " outs, ", 
                                     ifelse(!is.na(.data$on_1b), "1b", "_"), 
                                     ifelse(!is.na(.data$on_2b), "2b", "_"), 
                                     ifelse(!is.na(.data$on_3b), "3b", "_")))
    
    re_table <- run_expectancy_table(df, level = "pitch")
    
    df <- df %>% left_join(re_table, by = "count_base_out_state")
    
    df <- df %>% 
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(
        next_count_base_out_state = dplyr::lead(.data$count_base_out_state)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(re_table, by = c("next_count_base_out_state" = "count_base_out_state")) %>%
      dplyr::rename(
        next_avg_re = .data$avg_re.y, 
        avg_re = .data$avg_re.x) %>%
      dplyr::mutate(
        next_avg_re = ifelse(is.na(.data$next_avg_re),
                             0, .data$next_avg_re),
        change_re = .data$next_avg_re - .data$avg_re,
        runs_scored_on_pitch = ifelse(is.na(.data$runs_scored_on_pitch), 0, .data$runs_scored_on_pitch),
        re24 = .data$change_re + .data$runs_scored_on_pitch) %>%
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot)
  }
  
  return(df)
}

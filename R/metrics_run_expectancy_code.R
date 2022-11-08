#' @rdname run_expectancy_code
#' @title **Generate run expectancy and related measures from Baseball Savant data**
#'
#' @description These functions allow a user to generate run expectancy and related measures and variables from Baseball Savant data. Measures and variables will be added to the data frame.
#' @param df A data frame generated from Baseball Savant.
#' @param level Whether you want run expectancy calculated at the plate appearance or pitch level. Defaults to plate appearance.
#' @return Returns a tibble with the following columns:
#' 
#'  |col_name                        |types     |
#'  |:-------------------------------|:---------|
#'  |pitch_type                      |character |
#'  |game_date                       |Date      |
#'  |release_speed                   |numeric   |
#'  |release_pos_x                   |numeric   |
#'  |release_pos_z                   |numeric   |
#'  |player_name                     |character |
#'  |batter                          |numeric   |
#'  |pitcher                         |numeric   |
#'  |events                          |character |
#'  |description                     |character |
#'  |spin_dir                        |logical   |
#'  |spin_rate_deprecated            |logical   |
#'  |break_angle_deprecated          |logical   |
#'  |break_length_deprecated         |logical   |
#'  |zone                            |numeric   |
#'  |des                             |character |
#'  |game_type                       |character |
#'  |stand                           |character |
#'  |p_throws                        |character |
#'  |home_team                       |character |
#'  |away_team                       |character |
#'  |type                            |character |
#'  |hit_location                    |integer   |
#'  |bb_type                         |character |
#'  |balls                           |integer   |
#'  |strikes                         |integer   |
#'  |game_year                       |integer   |
#'  |pfx_x                           |numeric   |
#'  |pfx_z                           |numeric   |
#'  |plate_x                         |numeric   |
#'  |plate_z                         |numeric   |
#'  |on_3b                           |numeric   |
#'  |on_2b                           |numeric   |
#'  |on_1b                           |numeric   |
#'  |outs_when_up                    |integer   |
#'  |inning                          |numeric   |
#'  |inning_topbot                   |character |
#'  |hc_x                            |numeric   |
#'  |hc_y                            |numeric   |
#'  |tfs_deprecated                  |logical   |
#'  |tfs_zulu_deprecated             |logical   |
#'  |fielder_2                       |numeric   |
#'  |umpire                          |logical   |
#'  |sv_id                           |character |
#'  |vx0                             |numeric   |
#'  |vy0                             |numeric   |
#'  |vz0                             |numeric   |
#'  |ax                              |numeric   |
#'  |ay                              |numeric   |
#'  |az                              |numeric   |
#'  |sz_top                          |numeric   |
#'  |sz_bot                          |numeric   |
#'  |hit_distance_sc                 |numeric   |
#'  |launch_speed                    |numeric   |
#'  |launch_angle                    |numeric   |
#'  |effective_speed                 |numeric   |
#'  |release_spin_rate               |numeric   |
#'  |release_extension               |numeric   |
#'  |game_pk                         |numeric   |
#'  |pitcher_1                       |numeric   |
#'  |fielder_2_1                     |numeric   |
#'  |fielder_3                       |numeric   |
#'  |fielder_4                       |numeric   |
#'  |fielder_5                       |numeric   |
#'  |fielder_6                       |numeric   |
#'  |fielder_7                       |numeric   |
#'  |fielder_8                       |numeric   |
#'  |fielder_9                       |numeric   |
#'  |release_pos_y                   |numeric   |
#'  |estimated_ba_using_speedangle   |numeric   |
#'  |estimated_woba_using_speedangle |numeric   |
#'  |woba_value                      |numeric   |
#'  |woba_denom                      |integer   |
#'  |babip_value                     |integer   |
#'  |iso_value                       |integer   |
#'  |launch_speed_angle              |integer   |
#'  |at_bat_number                   |numeric   |
#'  |pitch_number                    |numeric   |
#'  |pitch_name                      |character |
#'  |home_score                      |numeric   |
#'  |away_score                      |numeric   |
#'  |bat_score                       |numeric   |
#'  |fld_score                       |numeric   |
#'  |post_away_score                 |numeric   |
#'  |post_home_score                 |numeric   |
#'  |post_bat_score                  |numeric   |
#'  |post_fld_score                  |numeric   |
#'  |if_fielding_alignment           |character |
#'  |of_fielding_alignment           |character |
#'  |spin_axis                       |numeric   |
#'  |delta_home_win_exp              |numeric   |
#'  |delta_run_exp                   |numeric   |
#'  |final_pitch_game                |numeric   |
#'  |final_pitch_at_bat              |numeric   |
#'  |runs_scored_on_pitch            |numeric   |
#'  |bat_score_after                 |numeric   |
#'  |final_pitch_inning              |numeric   |
#'  |bat_score_start_inning          |numeric   |
#'  |bat_score_end_inning            |numeric   |
#'  |cum_runs_in_inning              |numeric   |
#'  |runs_to_end_inning              |numeric   |
#'  |count_base_out_state            |character |
#'  |avg_re                          |numeric   |
#'  |next_count_base_out_state       |character |
#'  |next_avg_re                     |numeric   |
#'  |change_re                       |numeric   |
#'  |re24                            |numeric   |
#'  
#' @importFrom stringr str_count
#' @importFrom rlang .data
#' @export
#' @examples \donttest{
#'   df <- statcast_search(start_date = "2016-04-06", end_date = "2016-04-15", 
#'                         playerid = 621043, player_type = 'batter') 
#'   try(run_expectancy_code(df, level = "plate appearances"))
#' }

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
    
    df <- df %>% 
      dplyr::left_join(re_table, by = "base_out_state")
    df <- df %>% dplyr::filter(.data$final_pitch_at_bat == 1) %>%
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(next_base_out_state = dplyr::lead(.data$base_out_state)) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(re_table, by = c("next_base_out_state" = "base_out_state")) %>%
      dplyr::rename(
        "next_avg_re" = "avg_re.y", 
        "avg_re" = "avg_re.x") %>%
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
    
    df <- df %>% 
      dplyr::left_join(re_table, by = "count_base_out_state")
    
    df <- df %>% 
      dplyr::arrange(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::group_by(.data$game_pk, .data$inning, .data$inning_topbot) %>%
      dplyr::mutate(
        next_count_base_out_state = dplyr::lead(.data$count_base_out_state)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(re_table, by = c("next_count_base_out_state" = "count_base_out_state")) %>%
      dplyr::rename(
        "next_avg_re" = "avg_re.y", 
        "avg_re" = "avg_re.x") %>%
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

run_expectancy_table <- function(df, level = "plate appearance") {
  
  if (level == "plate appearance") {
    
    df <- df %>%
      dplyr::filter(.data$final_pitch_at_bat == 1, .data$inning < 9) %>%
      dplyr::group_by(.data$base_out_state) %>%
      dplyr::summarise(avg_re = mean(.data$runs_to_end_inning, na.rm = TRUE)) %>%
      dplyr::arrange(desc(.data$avg_re))
  } else {
    df <- df %>%
      dplyr::filter(.data$inning < 9) %>%
      dplyr::group_by(.data$count_base_out_state) %>%
      dplyr::summarise(avg_re = mean(.data$runs_to_end_inning, na.rm = TRUE)) %>%
      dplyr::arrange(desc(.data$avg_re))
  }
  return(df)
}

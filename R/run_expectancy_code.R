#' Generate run expectancy and related measures and variables from Baseball Savant data
#'
#' These functions allow a user to generate run expectancy and related measures and variables from Baseball Savant data. Measures and variables will be added to the data frame and a run expectancy table will be assigned to the Global Environment.
#' @param df A data frame generated from Baseball Savant.
#' @param level Whether you want run expectancy calculated at the plate appearance or pitch level. Defaults to plate appearance.
#' @keywords MLB, sabermetrics
#' @importFrom stringr str_count
#' @export
#' @examples
#' \dontrun{run_expectancy_code(df, level = "plate appearances")}

run_expectancy_code <- function (df, level = "plate appearance")
{
  single_outs <- c("strikeout", "caught_stealing_2b", "pickoff_caught_stealing_2b",
                   "other_out", "caught_stealing_3b", "caught_stealing_home",
                   "field_out", "force_out", "pickoff_1b", "batter_interference",
                   "fielders_choice", "pickoff_2b", "pickoff_caught_stealing_3b",
                   "pickoff_caught_stealing_home")
  df <- df %>%
    dplyr::arrange(game_pk, at_bat_number, pitch_number) %>%
    dplyr::group_by(game_pk) %>%
    dplyr::mutate(final_pitch_game =
                    ifelse(pitch_number == max(pitch_number), 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(game_pk, at_bat_number, inning_topbot) %>%
    dplyr::mutate(final_pitch_at_bat =
                    ifelse(pitch_number == max(pitch_number), 1, 0)) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::arrange(game_pk, inning_topbot, at_bat_number, pitch_number) %>%
    dplyr::mutate(runs_scored_on_pitch = stringr::str_count(des, "scores"),
                  runs_scored_on_pitch = ifelse(events == "home_run", runs_scored_on_pitch + 1, runs_scored_on_pitch), bat_score_after = bat_score + runs_scored_on_pitch) %>%
    dplyr::arrange(game_pk, at_bat_number, pitch_number) %>%
    dplyr::mutate(final_pitch_inning = ifelse(final_pitch_at_bat == 1 & inning_topbot != lead(inning_topbot), 1, 0), final_pitch_inning = ifelse(is.na(final_pitch_inning), 1, final_pitch_inning))

  if (level == "plate appearance") {

    df <- df %>%
      dplyr::group_by(game_pk, inning, inning_topbot) %>%
      dplyr::mutate(bat_score_start_inning = min(bat_score),
                    bat_score_end_inning = max(bat_score), cum_runs_in_inning = cumsum(runs_scored_on_pitch), runs_to_end_inning = bat_score_end_inning - bat_score) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(base_out_state = paste(outs_when_up, " outs, ", ifelse(!is.na(.$on_1b), "1b", "_"), ifelse(!is.na(.$on_2b),  "2b", "_"), ifelse(!is.na(.$on_3b), "3b", "_")))

    re_table <- run_expectancy_table(df)

    df <- df %>% left_join(re_table, by = "base_out_state")
    df <- df %>% dplyr::filter(final_pitch_at_bat == 1) %>%
      dplyr::arrange(game_pk, inning, inning_topbot) %>%
      dplyr::group_by(game_pk, inning, inning_topbot) %>%
      dplyr::mutate(next_base_out_state = dplyr::lead(base_out_state)) %>%
      dplyr::ungroup() %>% dplyr::left_join(re_table,
                                            by = c(next_base_out_state = "base_out_state")) %>%
      dplyr::rename(next_avg_re = avg_re.y, avg_re = avg_re.x) %>%
      dplyr::mutate(next_avg_re = ifelse(is.na(next_avg_re),
                                         0, next_avg_re),
                    change_re = next_avg_re - avg_re,
                    re24 = change_re + runs_scored_on_pitch) %>%
      dplyr::arrange(game_pk, inning, inning_topbot)
  }
  else {
    df <- df %>%
      dplyr::group_by(game_pk, inning, inning_topbot) %>%
      dplyr::mutate(bat_score_start_inning = min(bat_score),
                    bat_score_end_inning = max(bat_score),
                    cum_runs_in_inning = cumsum(runs_scored_on_pitch),
                    runs_to_end_inning = bat_score_end_inning -
                      bat_score) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(count_base_out_state = paste(balls, "-", strikes, ", ", outs_when_up, " outs, ", ifelse(!is.na(.$on_1b), "1b", "_"), ifelse(!is.na(.$on_2b), "2b", "_"), ifelse(!is.na(.$on_3b), "3b", "_")))

    re_table <- run_expectancy_table(df, level = "pitch")

    df <- df %>% left_join(re_table, by = "count_base_out_state")

    df <- df %>% dplyr::arrange(game_pk, inning, inning_topbot) %>%
      dplyr::group_by(game_pk, inning, inning_topbot) %>%
      dplyr::mutate(next_count_base_out_state =
                      dplyr::lead(count_base_out_state)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(re_table,
                       by =
                         c(next_count_base_out_state =
                             "count_base_out_state")) %>%
      dplyr::rename(next_avg_re = avg_re.y, avg_re = avg_re.x) %>%
      dplyr::mutate(next_avg_re = ifelse(is.na(next_avg_re),
                                         0, next_avg_re),
                    change_re = next_avg_re - avg_re,
                    runs_scored_on_pitch =
                      ifelse(is.na(runs_scored_on_pitch),
                             0,
                             runs_scored_on_pitch)) %>%
      dplyr::mutate(re24 = change_re + runs_scored_on_pitch) %>%
      dplyr::arrange(game_pk, inning, inning_topbot)
  }

  assign("run_expectancy_state_table", re_table, envir = .GlobalEnv)

  return(df)
}

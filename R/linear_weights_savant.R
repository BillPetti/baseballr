#' Generate linear weight values for events using Baseball Savant data
#'
#' This function allows a user to generate linear weight values for events using Baseball Savant data. Output includes both linear weights above average and linear weights above outs for home runs, triples, doubles, singles, walks, hit by pitches, and outs.
#' @param df A data frame generated from Baseball Savant that has been run through the \code{\link{run_expectancy_code}} function.
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' \dontrun{linear_weights_savant(df)}

linear_weights_savant <- function(df) {

  non_outs <- c("home_run", "triple", "double", "single", "walk", "hit_by_pitch", "pickoff_2B", "caught_stealing_2b", "caught_stealing_3b", "caught_stealing_home", "pickoff_1b", "pickoff_3b", "pickoff_caught_stealing_2b", "pickoff_caught_stealing_home", "catcher_interf", "batter_interference")

  df <- df %>%
    dplyr::filter(final_pitch_at_bat == 1)

  linear_wght_outs <- df %>%
    dplyr::filter(!events %in% non_outs) %>%
    dplyr::summarise(outs = round(mean(re24, na.rm = TRUE), 2))

  linear_above_outs <- df %>%
    dplyr::filter(events %in% c("home_run", "triple", "double", "single", "walk", "hit_by_pitch")) %>%
    dplyr::group_by(events) %>%
    dplyr::summarise(linear_weights_above_average =
                round(mean(re24, na.rm = TRUE), 2)) %>%
    dplyr::add_row(events = "outs", linear_weights_above_average =
              linear_wght_outs$outs) %>%
    dplyr::arrange(desc(linear_weights_above_average)) %>%
    dplyr::mutate(linear_weights_above_outs =
             linear_weights_above_average + abs(linear_wght_outs$outs))

  linear_above_outs
}

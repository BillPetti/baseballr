#' Generate run expectancy tables from Baseball Savant data
#'
#' These functions allow a user to generate run expectancy tables from Baseball Savant data. Tables are automatically assigned to the Global Environment.
#' @param df A data frame generated from Baseball Savant that has been formatted using the run_expectancy_code() function.
#' @param level Whether you want run expectancy calculated at the plate appearance or pitch level. Defaults to plate appearance.
#' @keywords MLB, sabermetrics
#' @importFrom dplyr filter group_by summarise arrange mutate
#' @export
#' @examples
#' \dontrun{run_expectancy_table(df, level = "plate appearances")}

run_expectancy_table <- function(df, level = "plate appearance") {

  if (level == "plate appearance") {

    df <- df %>%
      dplyr::filter(final_pitch_at_bat == 1, inning < 9) %>%
      dplyr::group_by(base_out_state) %>%
      dplyr::summarise(avg_re = mean(runs_to_end_inning, na.rm = TRUE)) %>%
      dplyr::arrange(desc(avg_re))
  } else {
    df <- df %>%
      dplyr::filter(inning < 9) %>%
      dplyr::group_by(count_base_out_state) %>%
      dplyr::summarise(avg_re = mean(runs_to_end_inning, na.rm = TRUE)) %>%
      dplyr::arrange(desc(avg_re))
  }
  df
}

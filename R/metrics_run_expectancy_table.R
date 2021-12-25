#' @title **Generate run expectancy tables from Baseball Savant data**
#'
#' @description These functions allow a user to generate run expectancy tables from Baseball Savant data.
#' @param df A data frame generated from Baseball Savant that has been formatted using the run_expectancy_code() function.
#' @param level Whether you want run expectancy calculated at the plate appearance or pitch level. Defaults to plate appearance.
#' @importFrom rlang .data
#' @export
#' @details
#' ```r
#' run_expectancy_table(df, level = "plate appearances")
#' ````

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

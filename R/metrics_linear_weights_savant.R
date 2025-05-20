#' @rdname linear_weights_savant
#' @title **Generate linear weight values for events using Baseball Savant data**
#' @description 
#' This function allows a user to generate linear weight values for events using Baseball Savant data. Output includes both linear weights above average and linear weights above outs for home runs, triples, doubles, singles, walks, hit by pitches, and outs.
#' @param df A data frame generated from Baseball Savant that has been run through
#' the ```run_expectancy_code()``` function.
#' @param level Whether to calculate linear weights the plate appearance or pitch
#' level. Defaults to 'plate appearance'.
#' @return Returns a tibble with the following columns:
#' 
#'   |col_name                     |types     |
#'   |:----------------------------|:---------|
#'   |events                       |character |
#'   |linear_weights_above_average |numeric   |
#'   |linear_weights_above_outs    |numeric   |
#'   
#' @export 
#' @examples \donttest{
#'  try({
#'    df <- statcast_search(start_date = "2016-04-06", end_date = "2016-04-15", 
#'                          playerid = 621043, player_type = 'batter') 
#'    df <- run_expectancy_code(df, level = "plate appearances")
#'    linear_weights_savant(df, level = "plate appearance")
#'  })
#' }

linear_weights_savant <- function (df, level = "plate appearance"){

  if(level == "plate appearance") {

    non_outs <- c("home_run", "triple", "double", "single",
                  "walk", "hit_by_pitch", "pickoff_2B", "caught_stealing_2b",
                  "caught_stealing_3b", "caught_stealing_home", "pickoff_1b",
                  "pickoff_3b", "pickoff_caught_stealing_2b",
                  "pickoff_caught_stealing_home", "catcher_interf",
                  "batter_interference")

    df <- df %>%
      dplyr::filter(.data$final_pitch_at_bat == 1)

    linear_wght_outs <- df %>%
      dplyr::filter(!.data$events %in% non_outs) %>%
      dplyr::summarise(
        outs = round(mean(.data$re24, na.rm = TRUE),2))
    linear_above_outs <- df %>%
      dplyr::filter(.data$events %in% c("home_run", "triple", "double", "single", "walk", "hit_by_pitch")) %>%
      dplyr::group_by(.data$events) %>%
      dplyr::summarise(linear_weights_above_average =
                         round(mean(.data$re24,na.rm = TRUE), 2)) %>%
      dplyr::add_row(
        events = "outs",
        linear_weights_above_average = linear_wght_outs$outs) %>%
      dplyr::arrange(desc(.data$linear_weights_above_average)) %>%
      dplyr::mutate(
        linear_weights_above_outs = .data$linear_weights_above_average +abs(linear_wght_outs$outs))
    return(linear_above_outs)
  }
  else {

    df <- df %>%
      dplyr::mutate(events = ifelse(is.na(.data$events) | .data$events == "", .data$type, .data$events)) %>%
      dplyr::mutate(events = ifelse(.data$events == "B", "ball",
                             ifelse(.data$events == "S", "strikes", .data$events)))

    non_outs <- c("ball", "strikes", "home_run", "triple", "double", "single",
                  "walk", "hit_by_pitch", "pickoff_2B", "caught_stealing_2b",
                  "caught_stealing_3b", "caught_stealing_home", "pickoff_1b",
                  "pickoff_3b", "pickoff_caught_stealing_2b",
                  "pickoff_caught_stealing_home", "catcher_interf",
                  "batter_interference")

    linear_wght_outs <- df %>%
      dplyr::filter(!.data$events %in% non_outs) %>%
      dplyr::summarise(outs = round(mean(.data$re24, na.rm = TRUE),
                                    2))
    linear_above_outs <- df %>%
      dplyr::filter(.data$events %in% c("home_run", "triple", "double", "single", "walk", "hit_by_pitch", "ball", "strikes")) %>%
      dplyr::group_by(.data$events) %>%
      dplyr::summarise(linear_weights_above_average =
                         round(mean(.data$re24,na.rm = TRUE), 2)) %>%
      dplyr::add_row(events = "outs",
                     linear_weights_above_average = linear_wght_outs$outs) %>%
      dplyr::arrange(desc(.data$linear_weights_above_average)) %>%
      dplyr::mutate(linear_weights_above_outs = .data$linear_weights_above_average +
                      abs(linear_wght_outs$outs))
    return(linear_above_outs)

  }
}

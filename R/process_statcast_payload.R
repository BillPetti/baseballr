#' Process Baseball Savant CSV payload
#'
#' This is a helper function for all scrape_statcast_savant functions.
#' The function processes the initial csv payload acquired from
#' baseballsavant to ensure consistency in formattting across downloads
#' @param payload payload from a Baseball Savant request, e.g.
#' from \code{\link[readr]{read_csv}}
#' @keywords MLB, sabermetrics, Statcast
#' @importFrom dplyr mutate_at mutate_
#' @export
#' @examples
#' \dontrun{
#' process_statcast_payload(payload)
#' }

process_statcast_payload <- function(payload) {

  # Clean up formatting of Baseball Savant download

  as_numeric_na <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  na_to_999999999 <- function(x) {
    ifelse(is.na(x), 999999999, x)
  }

  payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
  payload$des <- as.character(payload$des)
  payload$game_pk <- as.character(payload$game_pk) %>% as_numeric_na()
  payload$hc_x <- as.character(payload$hc_x) %>% as_numeric_na()
  payload$hc_y <- as.character(payload$hc_y) %>% as_numeric_na()
  payload$on_1b <- as.character(payload$on_1b) %>% as_numeric_na()
  payload$on_2b <- as.character(payload$on_2b) %>% as_numeric_na()
  payload$on_3b <- as.character(payload$on_3b) %>% as_numeric_na()
  payload$release_pos_x <- as.character(payload$release_pos_x) %>% as_numeric_na()
  payload$release_pos_y <- as.character(payload$release_pos_x) %>% as_numeric_na()
  payload$release_pos_z <- as.character(payload$release_pos_z) %>% as_numeric_na()
  payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as_numeric_na()
  payload$launch_speed <- as.character(payload$launch_speed) %>% as_numeric_na()
  payload$launch_angle <- as.character(payload$launch_angle) %>% as_numeric_na()
  payload$pfx_x <- as.character(payload$pfx_x) %>% as_numeric_na()
  payload$pfx_z <- as.character(payload$pfx_z) %>% as_numeric_na()
  payload$plate_x <- as.character(payload$plate_x) %>% as_numeric_na()
  payload$plate_z <- as.character(payload$plate_z) %>% as_numeric_na()
  payload$vx0 <- as.character(payload$vx0) %>% as_numeric_na()
  payload$vy0 <- as.character(payload$vy0) %>% as_numeric_na()
  payload$vz0 <- as.character(payload$vz0) %>% as_numeric_na()
  payload$ax <- as.character(payload$ax) %>% as_numeric_na()
  payload$az <- as.character(payload$az) %>% as_numeric_na()
  payload$ay <- as.character(payload$ay) %>% as_numeric_na()
  payload$sz_bot <- as.character(payload$sz_bot) %>% as_numeric_na()
  payload$sz_top <- as.character(payload$sz_top) %>% as_numeric_na()
  payload$effective_speed <- as.character(payload$effective_speed) %>% as_numeric_na()
  payload$release_speed <- as.character(payload$release_speed) %>% as_numeric_na()
  payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as_numeric_na()
  payload$release_extension <- as.character(payload$release_extension) %>% as_numeric_na()
  payload$pitch_name <- as.character(payload$pitch_name)
  payload$home_score <- as.character(payload$home_score) %>% as_numeric_na()
  payload$away_score <- as.character(payload$away_score) %>% as_numeric_na()
  payload$bat_score	<- as.character(payload$bat_score) %>% as_numeric_na()
  payload$fld_score <- as.character(payload$fld_score) %>% as_numeric_na()
  payload$post_away_score <- as.character(payload$post_away_score) %>% as_numeric_na()
  payload$post_home_score	<- as.character(payload$post_home_score) %>% as_numeric_na()
  payload$post_bat_score <- as.character(payload$post_bat_score) %>% as_numeric_na()
  payload$post_fld_score <- as.character(payload$post_fld_score) %>% as_numeric_na()
  payload$zone <- as.character(payload$zone) %>% as_numeric_na()

  # Format player IDs as character

  cols_to_transform <- c("batter", "pitcher", "fielder_2", "pitcher_1", "fielder_2_1",
                         "fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                         "fielder_8", "fielder_9")

  payload <- payload %>%
    dplyr::mutate(dplyr::across(cols_to_transform, as.character)) %>%
    dplyr::mutate(dplyr::across(cols_to_transform, as_numeric_na)) %>%
    dplyr::mutate(dplyr::across(cols_to_transform, na_to_999999999))

  # Create a specific variable for barrels

  payload <- payload %>%
    dplyr::mutate(barrel = ifelse(launch_speed_angle == 6, 1, 0))
  # dplyr::mutate_(
  # barrel = ~ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0)

  return(payload)

}

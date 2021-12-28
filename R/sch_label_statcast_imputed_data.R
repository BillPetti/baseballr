
#' @title **Label Statcast data as imputed**
#' 
#' @description Based on a series of heuristics, this function attempts to 
#' label Statcast data for which the launch angle and speed 
#' have been imputed. 
#' 
#' @param statcast_df A dataframe containing Statcast batted ball data
#' @param impute_file A CSV file giving the launch angle, launch speed, 
#' \code{bb_type}, events fields to label
#' as imputed. if NULL then it's read from the \code{extdata} folder of the package.
#' @param inverse_precision inverse of how many digits to truncate the launch angle 
#' and speed to for comparison. Default is \code{10000}, i.e. keep 4 digits of precision.
#' @return A copy of the input dataframe with a new column \code{imputed} appended. imputed
#' is 1 if launch angle and launch speed are likely imputed, 0 otherwise.
#' @return Returns a data frame with the following columns
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
#'  |sv_id                           |logical   |
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
#'  |ila                             |integer   |
#'  |ils                             |integer   |
#'  |imputed                         |numeric   |
#' @export
#' @examples
#' \donttest{
#'   statcast_df <- statcast_search("2017-05-01", "2017-05-02")
#'   sc_df <- label_statcast_imputed_data(statcast_df)
#'   mean(sc_df$imputed)
#' }
label_statcast_imputed_data <- function(statcast_df, impute_file = NULL, 
                                        inverse_precision = 10000) {

  if (is.null(impute_file)) {
    imputed_df <- baseballr::statcast_impute
  } else {
    imputed_df <- suppressMessages(data.table::fread(impute_file))
  }
  
  imputed_df$imputed <- 1
  tmp <- statcast_df %>% 
    dplyr::mutate(
      ila = as.integer(.data$launch_angle * inverse_precision), 
      ils = as.integer(.data$launch_speed * inverse_precision)) %>% 
    dplyr::left_join(imputed_df, by = c("ils", "ila", "bb_type", "events"))
  tmp$imputed <- ifelse(is.na(tmp$imputed), 0, 1)
  return(tmp)
}

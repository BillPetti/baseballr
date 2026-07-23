
cols <- c(
  "pitch_type", "game_date", "release_speed",
  "release_pos_x", "release_pos_z", "player_name",
  "batter", "pitcher", "events", "description", "spin_dir",
  "spin_rate_deprecated", "break_angle_deprecated",
  "break_length_deprecated", "zone", "des", "game_type", "stand",
  "p_throws", "home_team", "away_team", "type", "hit_location", 
  "bb_type", "balls", "strikes", "game_year", "pfx_x", "pfx_z",
  "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up",
  "inning", "inning_topbot", "hc_x", "hc_y", "tfs_deprecated", 
  "tfs_zulu_deprecated", "fielder_2", "umpire", "sv_id", 
  "vx0", "vy0", "vz0", "ax", "ay", "az", "sz_top", "sz_bot", 
  "hit_distance_sc", "launch_speed", "launch_angle", "effective_speed",
  "release_spin_rate", "release_extension", "game_pk",
  "fielder_3", "fielder_4",
  "fielder_5", "fielder_6", "fielder_7", "fielder_8", 
  "fielder_9", "release_pos_y", "estimated_ba_using_speedangle",
  "estimated_woba_using_speedangle", "woba_value",
  "woba_denom", "babip_value", "iso_value", "launch_speed_angle",
  "at_bat_number", "pitch_number", "pitch_name", "home_score",
  "away_score", "bat_score", "fld_score", "post_away_score", 
  "post_home_score", "post_bat_score", "post_fld_score",
  "if_fielding_alignment", "of_fielding_alignment",
  "spin_axis", "delta_home_win_exp", "delta_run_exp", "bat_speed", "swing_length",
  "miss_distance"
)

# Subset-direction assertion (including the newer bat_speed/swing_length
# columns) guards the length-tolerant column assignment that keeps
# statcast_search() from breaking when Baseball Savant adds columns
# (#337, #354, #371, #390).
test_that("Statcast Search", {
  skip_statcast_test()
  skip_on_cran()

  x <- statcast_search(start_date = "2022-11-04",
                       end_date = "2022-11-06")

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from Baseball Savant at test time")
  }

  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})

# Baseball Savant exports blank fields as empty strings. Numeric columns coerce
# to NA for free, but character columns (events, des, description, ...) used to
# keep "" -- which downstream helpers such as metrics_linear_weights_savant()
# do not treat as missing (#275). This is a pure, network-free parse test.
test_that("process_statcast_payload normalizes empty character fields to NA (#275)", {
  # csv_from_url() returns a data.table, so exercise that class here -- a plain
  # data.frame would not catch class-specific indexing bugs.
  payload <- data.table::as.data.table(as.data.frame(
    matrix("", nrow = 2, ncol = length(cols), dimnames = list(NULL, cols)),
    stringsAsFactors = FALSE
  ))
  payload$game_date   <- c("2022-06-01", "2022-06-01")
  payload$events      <- c("single", "")            # blank PA outcome -> NA
  payload$des         <- c("", "Foul.")             # blank description -> NA
  payload$description <- c("hit_into_play", "foul")

  # Input is a data.table (as from csv_from_url); convert the result to a plain
  # data.frame only for convenient column-wise inspection below.
  out <- as.data.frame(suppressWarnings(process_statcast_payload(payload)))

  # Blank coerced to NA, real values preserved.
  expect_true(is.na(out$events[2]))
  expect_identical(out$events[1], "single")
  expect_true(is.na(out$des[1]))
  expect_identical(out$des[2], "Foul.")

  # No character column should retain a whitespace-only value.
  char_cols <- names(out)[vapply(out, is.character, logical(1))]
  has_blank <- vapply(
    out[char_cols],
    function(x) any(!is.na(x) & trimws(x) == ""),
    logical(1)
  )
  expect_false(any(has_blank))
})

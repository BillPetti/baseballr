# **(legacy) Query Baseball Savant Leaderboards**

**(legacy) Query Baseball Savant Leaderboards**

## Usage

``` r
scrape_savant_leaderboards(
  leaderboard = "exit_velocity_barrels",
  year = 2020,
  abs = 50,
  min_pa = "q",
  min_pitches = 100,
  min_throws = 100,
  min_field = "q",
  min_run = 0,
  player_type = "batter",
  fielding_type = "player",
  oaa_position = "",
  oaa_roles = "",
  team = "",
  arsenal_type = "n_",
  run_type = "raw",
  min2b = 5,
  min3b = 0,
  position = "",
  bats = "",
  hand = ""
)
```

## Arguments

- leaderboard:

  The type of leaderboard to retrieve, input as a string. Current
  options include exit_velocity_barrels, expected_statistics,
  pitch_arsenal, outs_above_average, directional_oaa, catch_probability,
  pop_time, sprint_speed, and running_splits_90_ft, arm_strength.

- year:

  The season for which you want data.

- abs:

  The minimum number of batted balls. Applies only to
  exit_velocity_barrels leaderboards.

- min_pa:

  Minimum number of plate appearances. Can be a number or 'q' for
  qualified batters.

- min_pitches:

  Minimum number of pitches thrown.

- min_throws:

  Minimum number of throwing opportunities.

- min_field:

  Minimum number of fieding opportunities.

- min_run:

  Minimum number of running opportunities.

- player_type:

  One of either 'batter' or pitcher. For the expected_statistics
  leaderboard, 'batter-team' and 'pitcher-team' are also available.

- fielding_type:

  One of either 'player' or 'team'.

- oaa_position:

  Can be either the number position of a player or 'if' or 'of' for
  position categories.

- oaa_roles:

  Can be either the number position of a player or 'if' or 'of' for
  position categories.

- team:

  An abbreviation for a team. Can be left blank.

- arsenal_type:

  One of either 'n\_', 'avg_spin', or 'avg_speed'.

- run_type:

  One of either 'percent' or 'raw'.

- min2b:

  The minimum number of throwing attempts to second base.

- min3b:

  The minimum number of throwing attempts to third base.

- position:

  The numeric position of the player. For DH use 10. Can be left blank.

- bats:

  The handedness of the batter. One of 'R' or 'L'. Can be left blank.

- hand:

  The handedness of the pitcher. One of 'R' or 'L'. Can be left blank.

## Value

Returns a tibble of Statcast leaderboard data.

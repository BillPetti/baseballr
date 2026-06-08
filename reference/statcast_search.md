# **Query Statcast by Date Range and Players**

This function allows you to query Statcast data as provided on
<https://baseballsavant.mlb.com>

## Usage

``` r
statcast_search(
  start_date = Sys.Date() - 1,
  end_date = Sys.Date(),
  playerid = NULL,
  player_type = "batter",
  ...
)

statcast_search.default(
  start_date = Sys.Date() - 1,
  end_date = Sys.Date(),
  playerid = NULL,
  player_type = "batter",
  ...
)

statcast_search_batters(start_date, end_date, batterid = NULL, ...)

statcast_search_pitchers(start_date, end_date, pitcherid = NULL, ...)
```

## Arguments

- start_date:

  Date of first game for which you want data. Format must be in
  YYYY-MM-DD format.

- end_date:

  Date of last game for which you want data. Format must be in
  YYYY-MM-DD format.

- playerid:

  The MLBAM ID for the player whose data you want to query.

- player_type:

  The player type. Can be `batter` or `pitcher`. Default is `batter`

- ...:

  currently ignored

- batterid:

  The MLBAM ID for the batter whose data you want to query.

- pitcherid:

  The MLBAM ID for the pitcher whose data you want to query.

## Value

Returns a tibble with Statcast data with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| pitch_type | character | Abbreviation of the pitch type thrown (e.g. FF, SL, CH). |
| game_date | Date | Date the game was played. |
| release_speed | numeric | Pitch velocity out of the hand (mph). |
| release_pos_x | numeric | Horizontal release position of the ball, catcher's perspective (feet). |
| release_pos_z | numeric | Vertical release position of the ball, catcher's perspective (feet). |
| player_name | character | Pitcher (or batter, by query) name, Last, First. |
| batter | numeric | MLBAM ID for the batter. |
| pitcher | numeric | MLBAM ID for the pitcher. |
| events | character | Plate-appearance outcome (e.g. single, strikeout, home_run). |
| description | character | Pitch-level result description (e.g. ball, called_strike, hit_into_play). |
| spin_dir | logical | Deprecated spin direction field, no longer populated. |
| spin_rate_deprecated | logical | Deprecated legacy spin-rate field, no longer populated. |
| break_angle_deprecated | logical | Deprecated legacy break-angle field, no longer populated. |
| break_length_deprecated | logical | Deprecated legacy break-length field, no longer populated. |
| zone | numeric | Strike-zone region the pitch crossed (1-14 Gameday zone). |
| des | character | Full text description of the play. |
| game_type | character | Type of game: R (regular), PO/F/D/L/W (postseason), S (spring). |
| stand | character | Side of the plate the batter is standing (L or R). |
| p_throws | character | Hand the pitcher throws with (L or R). |
| home_team | character | Home team abbreviation. |
| away_team | character | Away team abbreviation. |
| type | character | Pitch result code: B (ball), S (strike), X (in play). |
| hit_location | integer | Fielder position number that fielded the ball. |
| bb_type | character | Batted-ball type (ground_ball, line_drive, fly_ball, popup). |
| balls | integer | Ball count before the pitch. |
| strikes | integer | Strike count before the pitch. |
| game_year | integer | Season year of the game. |
| pfx_x | numeric | Horizontal pitch movement from the catcher's perspective (feet). |
| pfx_z | numeric | Vertical pitch movement from the catcher's perspective (feet). |
| plate_x | numeric | Horizontal position of the pitch crossing the plate (feet from center). |
| plate_z | numeric | Vertical position of the pitch crossing the plate (feet above ground). |
| on_3b | numeric | MLBAM ID of the runner on third base, if any. |
| on_2b | numeric | MLBAM ID of the runner on second base, if any. |
| on_1b | numeric | MLBAM ID of the runner on first base, if any. |
| outs_when_up | integer | Number of outs when the batter came to the plate. |
| inning | numeric | Inning number. |
| inning_topbot | character | Half of the inning (Top or Bot). |
| hc_x | numeric | Hit coordinate X on the field diagram. |
| hc_y | numeric | Hit coordinate Y on the field diagram. |
| tfs_deprecated | logical | Deprecated time-from-start field, no longer populated. |
| tfs_zulu_deprecated | logical | Deprecated Zulu time-from-start field, no longer populated. |
| umpire | logical | Deprecated umpire field, no longer populated. |
| sv_id | logical | Deprecated Sportvision/Statcast pitch identifier, no longer populated. |
| vx0 | numeric | Velocity of the pitch in the x-direction at y=50 ft (ft/s). |
| vy0 | numeric | Velocity of the pitch in the y-direction at y=50 ft (ft/s). |
| vz0 | numeric | Velocity of the pitch in the z-direction at y=50 ft (ft/s). |
| ax | numeric | Acceleration of the pitch in the x-direction at y=50 ft (ft/s^2). |
| ay | numeric | Acceleration of the pitch in the y-direction at y=50 ft (ft/s^2). |
| az | numeric | Acceleration of the pitch in the z-direction at y=50 ft (ft/s^2). |
| sz_top | numeric | Top of the batter's strike zone for the pitch (feet). |
| sz_bot | numeric | Bottom of the batter's strike zone for the pitch (feet). |
| hit_distance_sc | numeric | Statcast-measured projected distance of the batted ball (feet). |
| launch_speed | numeric | Exit velocity of the batted ball (mph). |
| launch_angle | numeric | Vertical launch angle of the batted ball (degrees). |
| effective_speed | numeric | Perceived velocity adjusted for release extension (mph). |
| release_spin_rate | numeric | Spin rate of the pitch at release (rpm). |
| release_extension | numeric | Distance toward the plate at release (feet). |
| game_pk | numeric | Unique MLB game identifier. |
| fielder_2 | numeric | MLBAM ID of the catcher. |
| fielder_3 | numeric | MLBAM ID of the first baseman. |
| fielder_4 | numeric | MLBAM ID of the second baseman. |
| fielder_5 | numeric | MLBAM ID of the third baseman. |
| fielder_6 | numeric | MLBAM ID of the shortstop. |
| fielder_7 | numeric | MLBAM ID of the left fielder. |
| fielder_8 | numeric | MLBAM ID of the center fielder. |
| fielder_9 | numeric | MLBAM ID of the right fielder. |
| release_pos_y | numeric | Release position of the ball toward the plate (feet). |
| estimated_ba_using_speedangle | numeric | Expected batting average based on exit velocity and launch angle. |
| estimated_woba_using_speedangle | numeric | Expected wOBA based on exit velocity and launch angle. |
| woba_value | numeric | wOBA value assigned to the event. |
| woba_denom | integer | wOBA denominator (plate-appearance weight) for the event. |
| babip_value | integer | BABIP value assigned to the event (0 or 1). |
| iso_value | integer | Isolated power value assigned to the event. |
| launch_speed_angle | integer | Batted-ball classification code (1-6) from exit velocity and angle. |
| at_bat_number | numeric | Sequential plate-appearance number within the game. |
| pitch_number | numeric | Pitch number within the plate appearance. |
| pitch_name | character | Full name of the pitch type (e.g. 4-Seam Fastball, Slider). |
| home_score | numeric | Home team score before the pitch. |
| away_score | numeric | Away team score before the pitch. |
| bat_score | numeric | Batting team score before the pitch. |
| fld_score | numeric | Fielding team score before the pitch. |
| post_away_score | numeric | Away team score after the pitch. |
| post_home_score | numeric | Home team score after the pitch. |
| post_bat_score | numeric | Batting team score after the pitch. |
| post_fld_score | numeric | Fielding team score after the pitch. |
| if_fielding_alignment | character | Infield defensive alignment (Standard, Strategic, Infield shift). |
| of_fielding_alignment | character | Outfield defensive alignment (Standard, Strategic, 4th outfielder). |
| spin_axis | numeric | Spin axis of the pitch as a clock-face angle (degrees). |
| delta_home_win_exp | numeric | Change in home team win expectancy on the play. |
| delta_run_exp | numeric | Change in run expectancy on the play. |
| bat_speed | numeric | Bat speed at the point of contact (mph). |
| swing_length | numeric | Length of the swing path to contact (feet). |
| estimated_slg_using_speedangle | numeric | Expected slugging based on exit velocity and launch angle. |
| delta_pitcher_run_exp | numeric | Change in run expectancy credited to the pitcher. |
| hyper_speed | numeric | Adjusted (90th-percentile) exit velocity (mph). |
| home_score_diff | integer | Home team score minus away team score before the pitch. |
| bat_score_diff | integer | Batting team score minus fielding team score before the pitch. |
| home_win_exp | numeric | Home team win expectancy before the play. |
| bat_win_exp | numeric | Batting team win expectancy before the play. |
| age_pit_legacy | integer | Pitcher age using the legacy calculation. |
| age_bat_legacy | integer | Batter age using the legacy calculation. |
| age_pit | integer | Pitcher age for the season. |
| age_bat | integer | Batter age for the season. |
| n_thruorder_pitcher | integer | Times through the order the pitcher is facing the lineup. |
| n_priorpa_thisgame_player_at_bat | integer | Number of prior plate appearances by the batter in the game. |
| pitcher_days_since_prev_game | integer | Days since the pitcher's previous game appearance. |
| batter_days_since_prev_game | integer | Days since the batter's previous game appearance. |
| pitcher_days_until_next_game | integer | Days until the pitcher's next game appearance. |
| batter_days_until_next_game | integer | Days until the batter's next game appearance. |
| api_break_z_with_gravity | numeric | Vertical pitch break including gravity (inches). |
| api_break_x_arm | numeric | Horizontal pitch break to the pitcher's arm side (inches). |
| api_break_x_batter_in | numeric | Horizontal pitch break toward/away from the batter (inches). |
| arm_angle | numeric | Pitcher's arm angle at release (degrees). |
| attack_angle | numeric | Angle of the bat's path at contact (degrees). |
| attack_direction | numeric | Horizontal direction of the swing at contact (degrees). |
| swing_path_tilt | numeric | Vertical tilt of the swing path (degrees). |
| intercept_ball_minus_batter_pos_x_inches | numeric | Horizontal offset of ball-bat intercept from batter position (inches). |
| intercept_ball_minus_batter_pos_y_inches | numeric | Depth offset of ball-bat intercept from batter position (inches). |

Returns a tibble with Statcast data.

Returns a tibble with the same Statcast pitch-level columns as
`statcast_search()`, filtered to the requested batter. See the
`statcast_search()` return value for the full column-by-column
reference.

Returns a tibble with the same Statcast pitch-level columns as
`statcast_search()`, filtered to the requested pitcher. See the
`statcast_search()` return value for the full column-by-column
reference.

## Examples

``` r
# \donttest{
  ### Harper
  try(statcast_search(start_date = "2022-10-06", 
                      end_date = "2022-10-16", 
                      playerid = 547180, 
                      player_type = 'batter'))
#> ── MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com 
#> ℹ Data updated: 2026-06-08 11:10:37 UTC
#> # A tibble: 82 × 118
#>    pitch_type game_date  release_speed release_pos_x release_pos_z
#>    <chr>      <date>             <dbl>         <dbl>         <dbl>
#>  1 FF         2022-10-15          96.4         -2.61          5.49
#>  2 CU         2022-10-15          83.3         -2.7           5.36
#>  3 FF         2022-10-15          96.2         -2.72          5.57
#>  4 FF         2022-10-15          96.1         -2.67          5.39
#>  5 FF         2022-10-15          95.8         -2.62          5.45
#>  6 FC         2022-10-15          93.4         -1.05          6.88
#>  7 FC         2022-10-15          93.1         -1.16          6.87
#>  8 FC         2022-10-15          92.3         -1.1           6.86
#>  9 FF         2022-10-15          95.8         -1.47          5.64
#> 10 FF         2022-10-15          98.8          2.13          5.77
#> # ℹ 72 more rows
#> # ℹ 113 more variables: player_name <chr>, batter <dbl>, pitcher <dbl>,
#> #   events <chr>, description <chr>, spin_dir <lgl>,
#> #   spin_rate_deprecated <lgl>, break_angle_deprecated <lgl>,
#> #   break_length_deprecated <lgl>, zone <dbl>, des <chr>,
#> #   game_type <chr>, stand <chr>, p_throws <chr>, home_team <chr>,
#> #   away_team <chr>, type <chr>, hit_location <int>, bb_type <chr>, …
  ### Framber
  try(statcast_search(start_date = "2022-10-06", 
                      end_date = "2022-10-16", 
                      playerid = 664285, 
                      player_type = 'pitcher'))
#> ── MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com 
#> ℹ Data updated: 2026-06-08 11:10:38 UTC
#> # A tibble: 92 × 118
#>    pitch_type game_date  release_speed release_pos_x release_pos_z
#>    <chr>      <date>             <dbl>         <dbl>         <dbl>
#>  1 CU         2022-10-13          78.1          1.09          6.06
#>  2 SI         2022-10-13          94.7          0.86          6.15
#>  3 CU         2022-10-13          79.4          0.95          6.02
#>  4 CU         2022-10-13          79.1          1.04          6.19
#>  5 CU         2022-10-13          81.4          0.93          6.06
#>  6 CU         2022-10-13          80.6          1.03          6.05
#>  7 SI         2022-10-13          95.4          0.71          6.19
#>  8 CU         2022-10-13          80.6          0.86          6.11
#>  9 CH         2022-10-13          91.6          0.73          6.19
#> 10 CU         2022-10-13          81            0.84          6.13
#> # ℹ 82 more rows
#> # ℹ 113 more variables: player_name <chr>, batter <dbl>, pitcher <dbl>,
#> #   events <chr>, description <chr>, spin_dir <lgl>,
#> #   spin_rate_deprecated <lgl>, break_angle_deprecated <lgl>,
#> #   break_length_deprecated <lgl>, zone <dbl>, des <chr>,
#> #   game_type <chr>, stand <chr>, p_throws <chr>, home_team <chr>,
#> #   away_team <chr>, type <chr>, hit_location <int>, bb_type <chr>, …
  ### Daily
  try(statcast_search(start_date = "2022-11-04", 
                      end_date = "2022-11-06"))
#> ── MLB Baseball Savant Statcast Search data from baseballsavant.mlb.com 
#> ℹ Data updated: 2026-06-08 11:10:38 UTC
#> # A tibble: 250 × 118
#>    pitch_type game_date  release_speed release_pos_x release_pos_z
#>    <chr>      <date>             <dbl>         <dbl>         <dbl>
#>  1 CU         2022-11-05          80.8          1.01          6.1 
#>  2 SL         2022-11-05          85.3          0.96          6.13
#>  3 SI         2022-11-05          95.7          0.73          6.3 
#>  4 SL         2022-11-05          84.5          1.09          6.04
#>  5 CU         2022-11-05          79.1          0.99          6.2 
#>  6 CH         2022-11-05          87.5          1.04          6.16
#>  7 SI         2022-11-05          94.3          0.99          6.26
#>  8 CU         2022-11-05          78.9          1.09          6.13
#>  9 CU         2022-11-05          81.1          1.25          6.18
#> 10 SI         2022-11-05          95.7          0.62          6.34
#> # ℹ 240 more rows
#> # ℹ 113 more variables: player_name <chr>, batter <dbl>, pitcher <dbl>,
#> #   events <chr>, description <chr>, spin_dir <lgl>,
#> #   spin_rate_deprecated <lgl>, break_angle_deprecated <lgl>,
#> #   break_length_deprecated <lgl>, zone <dbl>, des <chr>,
#> #   game_type <chr>, stand <chr>, p_throws <chr>, home_team <chr>,
#> #   away_team <chr>, type <chr>, hit_location <int>, bb_type <chr>, …
# }
# \donttest{
  try({
    correa <- statcast_search_batters(start_date = "2016-04-06",
      end_date = "2016-04-15", batterid = 621043)
    daily <- statcast_search_batters(start_date = "2016-04-06",
      end_date = "2016-04-06", batterid = NULL)
  })
# }
# \donttest{
  try({
    x <- statcast_search_pitchers(start_date = "2016-04-06",
      end_date = "2016-04-15", pitcherid = 592789)
    daily <- statcast_search_pitchers(start_date = "2016-04-06",
      end_date = "2016-04-06", pitcherid = NULL)
  })
# }
```

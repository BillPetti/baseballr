# **Query Baseball Savant Leaderboards**

This function allows you to read leaderboard data from BaseballSavant
directly into R as data frame.

## Usage

``` r
statcast_leaderboards(
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

Returns a tibble of Statcast leaderboard data with the following columns
(for leaderboard: 'exit_velocity_barrels'):

|  |  |  |
|----|----|----|
| col_name | types | description |
| year | numeric | Leaderboard season. |
| last_name, first_name | character | Player name as "Last, First". |
| player_id | integer | MLBAM player ID. |
| attempts | integer | Number of batted-ball events (attempts). |
| avg_hit_angle | numeric | Average launch angle on batted balls (degrees). |
| anglesweetspotpercent | numeric | Share of batted balls hit in the 8-32 degree sweet-spot range (percent). |
| max_hit_speed | numeric | Maximum exit velocity recorded (mph). |
| avg_hit_speed | numeric | Average exit velocity on batted balls (mph). |
| ev50 | numeric | Average of the top 50% of hardest-hit batted balls by exit velocity (mph). |
| fbld | numeric | Average exit velocity on fly balls and line drives (mph). |
| gb | numeric | Average exit velocity on ground balls (mph). |
| max_distance | integer | Maximum batted-ball distance (feet). |
| avg_distance | integer | Average batted-ball distance (feet). |
| avg_hr_distance | integer | Average home-run distance (feet). |
| ev95plus | integer | Number of batted balls hit at 95+ mph. |
| ev95percent | numeric | Share of batted balls hit at 95+ mph (percent). |
| barrels | integer | Number of barreled batted balls. |
| brl_percent | numeric | Barrels per batted-ball event (percent). |
| brl_pa | numeric | Barrels per plate appearance (percent). |

## Details

oaa_roles argument: 30 = 1B - Straight Up 31 = 1B - Towards 1B/2B Hole
32 = 1B - Close to Line 40 = 2B - Straight Up 41 = 2B - Shaded Towards
2B Bag 42 = 2B - Towards 1B/2B Hole 43 = 2B - Behind First Basemen 46 =
2B - Up the Middle 60 = SS - Straight Up 61 = SS - Towards 3B/SS Hole 62
= SS - Shaded Towards 2B Bag 64 = SS - Up the Middle 50 = 3B - Straight
Up 51 = 3B - Close to Line 52 = 3B - Towards 3B/SS Hole 77 = LF - Close
to Line 71 = LF - Leaning Left 70 = LF - Straight Up 72 = LF - Leaning
Right 78 = LF - LF Gap 87 = CF - LF Gap 81 = CF - Leaning Left 82 = CF -
Leaning Right 89 = CF - RF Gap 98 = RF - RF Gap 91 = RF - Leaning Left
90 = RF - Straight Up 92 = RF - Leaning Right 99 = RF - Close to Line

## Examples

``` r
# \donttest{
  try(statcast_leaderboards(leaderboard = "expected_statistics", year = 2018))
#> ── MLB Baseball Savant Statcast Leaderboards data from baseballsavant.ml
#> ℹ Data updated: 2026-06-09 20:45:06 UTC
#> # A tibble: 249 × 14
#>     year `last_name, first_name` player_id    pa   bip    ba est_ba
#>    <int> <chr>                       <int> <int> <int> <dbl>  <dbl>
#>  1  2018 Lindor, Francisco          596019   745   560 0.277  0.289
#>  2  2018 Turner, Trea               607208   740   534 0.271  0.273
#>  3  2018 Machado, Manny             592518   709   533 0.297  0.284
#>  4  2018 Hernández, César           514917   708   454 0.253  0.242
#>  5  2018 Merrifield, Whit           593160   707   526 0.304  0.281
#>  6  2018 Freeman, Freddie           518692   707   492 0.309  0.295
#>  7  2018 Bregman, Alex              608324   705   512 0.286  0.267
#>  8  2018 Stanton, Giancarlo         519317   705   416 0.266  0.237
#>  9  2018 Markakis, Nick             455976   705   552 0.297  0.293
#> 10  2018 Semien, Marcus             543760   703   510 0.255  0.245
#> # ℹ 239 more rows
#> # ℹ 7 more variables: est_ba_minus_ba_diff <dbl>, slg <dbl>,
#> #   est_slg <dbl>, est_slg_minus_slg_diff <dbl>, woba <dbl>,
#> #   est_woba <dbl>, est_woba_minus_woba_diff <dbl>
  try(statcast_leaderboards(leaderboard = "arm_strength", year = 2020))
#> ── MLB Baseball Savant Statcast Leaderboards data from baseballsavant.ml
#> ℹ Data updated: 2026-06-09 20:45:06 UTC
#> # A tibble: 183 × 27
#>     year fielder_name     player_id team_name primary_position
#>    <dbl> <chr>                <int> <lgl>                <int>
#>  1  2020 Santana, Carlos     467793 NA                       3
#>  2  2020 Freeman, Freddie    518692 NA                       3
#>  3  2020 Muncy, Max          571970 NA                       3
#>  4  2020 Olson, Matt         621566 NA                       3
#>  5  2020 Dozier, Hunter      641531 NA                       3
#>  6  2020 Canó, Robinson      429664 NA                       4
#>  7  2020 Solano, Donovan     456781 NA                       4
#>  8  2020 Altuve, Jose        514888 NA                       4
#>  9  2020 Hernández, César    514917 NA                       4
#> 10  2020 Segura, Jean        516416 NA                       4
#> # ℹ 173 more rows
#> # ℹ 22 more variables: primary_position_name <chr>, total_throws <int>,
#> #   total_throws_1b <int>, total_throws_2b <int>,
#> #   total_throws_3b <int>, total_throws_ss <int>,
#> #   total_throws_lf <int>, total_throws_cf <int>,
#> #   total_throws_rf <int>, total_throws_inf <int>,
#> #   total_throws_of <int>, max_arm_strength <dbl>, arm_1b <dbl>, …
# }
```

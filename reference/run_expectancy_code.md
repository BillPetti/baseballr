# **Generate run expectancy and related measures from Baseball Savant data**

These functions allow a user to generate run expectancy and related
measures and variables from Baseball Savant data. Measures and variables
will be added to the data frame.

## Usage

``` r
run_expectancy_code(df, level = "plate appearance")
```

## Arguments

- df:

  A data frame generated from Baseball Savant.

- level:

  Whether you want run expectancy calculated at the plate appearance or
  pitch level. Defaults to plate appearance.

## Value

Returns the input Baseball Savant data frame (the same Statcast
pitch-level columns produced by
[`statcast_search()`](https://billpetti.github.io/baseballr/reference/statcast_search.md)
– see that function's return value for the full column-by-column
reference) with run-expectancy columns appended. The appended columns
are:

|  |  |  |
|----|----|----|
| col_name | types | description |
| final_pitch_game | numeric | 1 if the row is the last pitch of the game, 0 otherwise. |
| final_pitch_at_bat | numeric | 1 if the row is the last pitch of the plate appearance, 0 otherwise. |
| runs_scored_on_pitch | numeric | Runs that scored on the pitch (parsed from the play description, plus the batter on a home run). |
| bat_score_after | numeric | Batting team score after the pitch. |
| final_pitch_inning | numeric | 1 if the row is the last pitch of the half-inning, 0 otherwise. |
| bat_score_start_inning | numeric | Batting team score at the start of the half-inning. |
| bat_score_end_inning | numeric | Batting team score at the end of the half-inning. |
| cum_runs_in_inning | numeric | Cumulative runs scored in the half-inning up to the pitch. |
| runs_to_end_inning | numeric | Runs scored from the current state to the end of the half-inning. |
| count_base_out_state | character | Count, outs, and base-occupancy state string (pitch-level run expectancy). |
| avg_re | numeric | Average run expectancy for the current base-out (or count-base-out) state. |
| next_count_base_out_state | character | Base-out (or count-base-out) state of the following event. |
| next_avg_re | numeric | Average run expectancy for the next state (0 at the end of an inning). |
| change_re | numeric | Change in run expectancy between the current and next state. |
| re24 | numeric | RE24: change in run expectancy plus runs scored on the pitch. |

At the plate-appearance level the analogous state columns are named
`base_out_state` and `next_base_out_state`.

## Examples

``` r
# \donttest{
 try({
   df <- statcast_search(start_date = "2016-04-06", end_date = "2016-04-15", 
                         playerid = 621043, player_type = 'batter') 
   run_expectancy_code(df, level = "plate appearances")
 })
#> # A tibble: 159 × 133
#>    pitch_type game_date  release_speed release_pos_x release_pos_z
#>    <chr>      <date>             <dbl>         <dbl>         <dbl>
#>  1 FF         2016-04-06          92.8         -2.25          6.89
#>  2 FF         2016-04-06          91.1         -2.32          6.83
#>  3 SL         2016-04-06          80.2         -2.46          6.94
#>  4 FF         2016-04-06          94.5         -2.37          6.74
#>  5 FF         2016-04-06          94           -1.98          6.77
#>  6 FC         2016-04-06          92.6         -2.35          6.62
#>  7 CH         2016-04-06          91.7         -2.25          6.64
#>  8 CH         2016-04-06          86.1         -2.07          6.93
#>  9 SI         2016-04-06          91.4         -1.89          6.71
#> 10 SI         2016-04-06          91.6         -1.73          6.8 
#> # ℹ 149 more rows
#> # ℹ 128 more variables: player_name <chr>, batter <dbl>, pitcher <dbl>,
#> #   events <chr>, description <chr>, spin_dir <lgl>,
#> #   spin_rate_deprecated <lgl>, break_angle_deprecated <lgl>,
#> #   break_length_deprecated <lgl>, zone <dbl>, des <chr>,
#> #   game_type <chr>, stand <chr>, p_throws <chr>, home_team <chr>,
#> #   away_team <chr>, type <chr>, hit_location <int>, bb_type <chr>, …
# }
```

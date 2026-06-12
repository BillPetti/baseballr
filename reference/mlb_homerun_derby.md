# **Retrieve Homerun Derby data**

**Retrieve Homerun Derby data**

## Usage

``` r
mlb_homerun_derby(game_pk)
```

## Arguments

- game_pk:

  The game_pk for which you want to return data

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_pk | integer | MLB game primary key for the Home Run Derby event. |
| event_name | character | Event name (e.g. 'All-Star Workout Day: Home Run Derby'). |
| event_date | character | Event date-time in ISO 8601 (e.g. '2017-07-11T00:00:00Z'). |
| event_type_code | character | Single-letter event type code (e.g. 'O'). |
| event_type_name | character | Event type name (e.g. 'Other'). |
| venue_id | integer | MLB venue id hosting the event. |
| venue_name | character | Venue name (e.g. 'Marlins Park'). |
| round | integer | Derby round number for the matchup. |
| batter | character | Full name of the batter for this swing record. |
| batter_id | integer | MLB player id of the batter. |
| batter_link | character | API relative link to the batter. |
| top_seed_complete | logical | Whether the top seed's turn in the matchup is complete. |
| top_seed_started | logical | Whether the top seed's turn in the matchup has started. |
| top_seed_winner | logical | Whether the top seed won the matchup. |
| bonus_time | logical | Whether the swing occurred during bonus time. |
| home_run | logical | Whether the swing was scored a home run. |
| tie_breaker | logical | Whether the swing occurred during a tie-breaker. |
| is_home_run | logical | Whether the recorded hit is a home run. |
| time_remaining | character | Time remaining on the clock when the swing occurred. |
| is_bonus_time | logical | Whether the swing counted toward bonus time. |
| is_tie_breaker | logical | Whether the swing counted toward a tie-breaker. |
| hit_data_launch_speed | integer | Exit velocity of the home run swing (mph). |
| hit_data_launch_angle | integer | Launch angle of the batted ball (degrees). |
| hit_data_total_distance | integer | Projected total distance of the batted ball (feet). |
| hit_data_coordinates_coord_x | numeric | Hit location x-coordinate on the field overlay. |
| hit_data_coordinates_coord_y | numeric | Hit location y-coordinate on the field overlay. |
| hit_data_coordinates_landing_pos_x | numeric | Landing position x-coordinate of the batted ball. |
| hit_data_coordinates_landing_pos_y | numeric | Landing position y-coordinate of the batted ball. |
| hit_data_trajectory_data_trajectory_polynomial_x | list | Polynomial coefficients of the x trajectory. |
| hit_data_trajectory_data_trajectory_polynomial_y | list | Polynomial coefficients of the y trajectory. |
| hit_data_trajectory_data_trajectory_polynomial_z | list | Polynomial coefficients of the z trajectory. |
| hit_data_trajectory_data_valid_time_interval | list | Valid time interval for the trajectory fit (seconds). |
| top_seed_seed | integer | Bracket seed number of the top seed. |
| top_seed_is_winner | logical | Whether the top seed is the matchup winner. |
| top_seed_is_complete | logical | Whether the top seed's turn is complete. |
| top_seed_is_started | logical | Whether the top seed's turn has started. |
| top_seed_num_home_runs | integer | Number of home runs hit by the top seed. |
| top_seed_player_id | integer | MLB player id of the top seed. |
| top_seed_player_full_name | character | Full name of the top seed. |
| top_seed_player_link | character | API relative link to the top seed player. |
| top_seed_top_derby_hit_data_launch_speed | integer | Top seed's hardest-hit exit velocity in the round (mph). |
| top_seed_top_derby_hit_data_total_distance | integer | Top seed's longest projected distance in the round (feet). |
| bottom_seed_complete | logical | Whether the bottom seed's turn in the matchup is complete. |
| bottom_seed_started | logical | Whether the bottom seed's turn in the matchup has started. |
| bottom_seed_winner | logical | Whether the bottom seed won the matchup. |
| bottom_seed_seed | integer | Bracket seed number of the bottom seed. |
| bottom_seed_is_winner | logical | Whether the bottom seed is the matchup winner. |
| bottom_seed_is_complete | logical | Whether the bottom seed's turn is complete. |
| bottom_seed_is_started | logical | Whether the bottom seed's turn has started. |
| bottom_seed_num_home_runs | integer | Number of home runs hit by the bottom seed. |
| bottom_seed_player_id | integer | MLB player id of the bottom seed. |
| bottom_seed_player_full_name | character | Full name of the bottom seed. |
| bottom_seed_player_link | character | API relative link to the bottom seed player. |
| bottom_seed_top_derby_hit_data_launch_speed | integer | Bottom seed's hardest-hit exit velocity in the round (mph). |
| bottom_seed_top_derby_hit_data_total_distance | integer | Bottom seed's longest projected distance in the round (feet). |
| venue_link | character | API relative link to the event venue. |
| is_multi_day | logical | Whether the event spans multiple days. |
| is_primary_calendar | logical | Whether the event is on the primary calendar. |
| file_code | character | Internal file code for the event. |
| event_number | integer | Event number identifier. |
| public_facing | logical | Whether the event is public facing. |

## Examples

``` r
# \donttest{
  try(mlb_homerun_derby(game_pk = 511101))
#> ── MLB Homerun Derby data from MLB.com ────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:24:51 UTC
#> # A tibble: 396 × 61
#>    game_pk event_name         event_date event_type_code event_type_name
#>      <int> <chr>              <chr>      <chr>           <chr>          
#>  1  511101 All-Star Workout … 2017-07-1… O               Other          
#>  2  511101 All-Star Workout … 2017-07-1… O               Other          
#>  3  511101 All-Star Workout … 2017-07-1… O               Other          
#>  4  511101 All-Star Workout … 2017-07-1… O               Other          
#>  5  511101 All-Star Workout … 2017-07-1… O               Other          
#>  6  511101 All-Star Workout … 2017-07-1… O               Other          
#>  7  511101 All-Star Workout … 2017-07-1… O               Other          
#>  8  511101 All-Star Workout … 2017-07-1… O               Other          
#>  9  511101 All-Star Workout … 2017-07-1… O               Other          
#> 10  511101 All-Star Workout … 2017-07-1… O               Other          
#> # ℹ 386 more rows
#> # ℹ 56 more variables: venue_id <int>, venue_name <chr>, round <int>,
#> #   batter <chr>, batter_id <int>, batter_link <chr>,
#> #   top_seed_complete <lgl>, top_seed_started <lgl>,
#> #   top_seed_winner <lgl>, bonus_time <lgl>, home_run <lgl>,
#> #   tie_breaker <lgl>, is_home_run <lgl>, time_remaining <chr>,
#> #   is_bonus_time <lgl>, is_tie_breaker <lgl>, …
# }
```

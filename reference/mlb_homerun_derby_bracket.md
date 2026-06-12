# **Retrieve Homerun Derby Bracket**

**Retrieve Homerun Derby Bracket**

## Usage

``` r
mlb_homerun_derby_bracket(game_pk)
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
| event_type_code | character | Single-letter event type code (e.g. 'O'). |
| event_type_name | character | Event type name (e.g. 'Other'). |
| event_date | character | Event date-time in ISO 8601 (e.g. '2017-07-11T00:00:00Z'). |
| venue_id | integer | MLB venue id hosting the event. |
| venue_name | character | Venue name (e.g. 'Marlins Park'). |
| venue_link | character | API relative link to the event venue. |
| is_multi_day | logical | Whether the event spans multiple days. |
| is_primary_calendar | logical | Whether the event is on the primary calendar. |
| file_code | character | Internal file code for the event. |
| event_number | integer | Event number identifier. |
| public_facing | logical | Whether the event is public facing. |
| round | integer | Derby bracket round number. |
| top_seed_complete | logical | Whether the top seed's turn in the matchup is complete. |
| top_seed_started | logical | Whether the top seed's turn in the matchup has started. |
| top_seed_winner | logical | Whether the top seed won the matchup. |
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

## Examples

``` r
# \donttest{
  try(mlb_homerun_derby_bracket(game_pk = 511101))
#> ── MLB Homerun Derby Bracket data from MLB.com ────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:23:55 UTC
#> # A tibble: 7 × 40
#>   game_pk event_name event_type_code event_type_name event_date venue_id
#>     <int> <chr>      <chr>           <chr>           <chr>         <int>
#> 1  511101 All-Star … O               Other           2017-07-1…     4169
#> 2  511101 All-Star … O               Other           2017-07-1…     4169
#> 3  511101 All-Star … O               Other           2017-07-1…     4169
#> 4  511101 All-Star … O               Other           2017-07-1…     4169
#> 5  511101 All-Star … O               Other           2017-07-1…     4169
#> 6  511101 All-Star … O               Other           2017-07-1…     4169
#> 7  511101 All-Star … O               Other           2017-07-1…     4169
#> # ℹ 34 more variables: venue_name <chr>, venue_link <chr>,
#> #   is_multi_day <lgl>, is_primary_calendar <lgl>, file_code <chr>,
#> #   event_number <int>, public_facing <lgl>, round <int>,
#> #   top_seed_complete <lgl>, top_seed_started <lgl>,
#> #   top_seed_winner <lgl>, top_seed_seed <int>,
#> #   top_seed_is_winner <lgl>, top_seed_is_complete <lgl>,
#> #   top_seed_is_started <lgl>, top_seed_num_home_runs <int>, …
# }
```

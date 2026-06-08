# **Find MLB Seasons**

**Find MLB Seasons**

## Usage

``` r
mlb_seasons(sport_id = 1, with_game_type_dates = TRUE)
```

## Arguments

- sport_id:

  The sport_id to return season information for.

- with_game_type_dates:

  with_game_type_dates to return season information

## Value

Returns a tibble with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| season_id | character | Season year identifier. |
| has_wildcard | logical | Whether the season has a wild card round. |
| pre_season_start_date | character | Pre-season start date. |
| pre_season_end_date | character | Pre-season end date. |
| season_start_date | character | Season start date. |
| spring_start_date | character | Spring training start date. |
| spring_end_date | character | Spring training end date. |
| regular_season_start_date | character | Regular season start date. |
| last_date1st_half | character | Last date of the first half. |
| all_star_date | character | All-Star Game date. |
| first_date2nd_half | character | First date of the second half. |
| regular_season_end_date | character | Regular season end date. |
| post_season_start_date | character | Post-season start date. |
| post_season_end_date | character | Post-season end date. |
| season_end_date | character | Season end date. |
| offseason_start_date | character | Off-season start date. |
| off_season_end_date | character | Off-season end date. |
| season_level_gameday_type | character | Season-level Gameday data feed type. |
| game_level_gameday_type | character | Game-level Gameday data feed type. |
| qualifier_plate_appearances | numeric | Plate appearances per team game to qualify. |
| qualifier_outs_pitched | integer | Outs pitched per team game to qualify. |

## Examples

``` r
# \donttest{
 try(mlb_seasons(sport_id = 1))
#> ── MLB Seasons data from MLB.com ──────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 11:09:25 UTC
#> # A tibble: 1 × 21
#>   season_id has_wildcard pre_season_start_date pre_season_end_date
#>   <chr>     <lgl>        <chr>                 <chr>              
#> 1 2026      TRUE         2026-01-01            2026-02-19         
#> # ℹ 17 more variables: season_start_date <chr>,
#> #   spring_start_date <chr>, spring_end_date <chr>,
#> #   regular_season_start_date <chr>, last_date1st_half <chr>,
#> #   all_star_date <chr>, first_date2nd_half <chr>,
#> #   regular_season_end_date <chr>, post_season_start_date <chr>,
#> #   post_season_end_date <chr>, season_end_date <chr>,
#> #   offseason_start_date <chr>, off_season_end_date <chr>, …
# }
```

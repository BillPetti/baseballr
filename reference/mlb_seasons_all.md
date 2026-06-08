# **Find MLB Seasons all**

**Find MLB Seasons all**

## Usage

``` r
mlb_seasons_all(
  sport_id = 1,
  division_id = NULL,
  league_id = NULL,
  with_game_type_dates = TRUE
)
```

## Arguments

- sport_id:

  The sport_id to return season information for.

- division_id:

  The division_id to return season information for.

- league_id:

  The league_id to return season information for.

- with_game_type_dates:

  with_game_type_dates to return season information for.

## Value

Returns a tibble with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| season_id | character | Season year identifier. |
| has_wildcard | logical | Whether the season has a wild card round. |
| pre_season_start_date | character | Pre-season start date. |
| season_start_date | character | Season start date. |
| regular_season_start_date | character | Regular season start date. |
| regular_season_end_date | character | Regular season end date. |
| season_end_date | character | Season end date. |
| offseason_start_date | character | Off-season start date. |
| off_season_end_date | character | Off-season end date. |
| season_level_gameday_type | character | Season-level Gameday data feed type. |
| game_level_gameday_type | character | Game-level Gameday data feed type. |
| qualifier_plate_appearances | numeric | Plate appearances per team game to qualify. |
| qualifier_outs_pitched | integer | Outs pitched per team game to qualify. |
| post_season_start_date | character | Post-season start date. |
| post_season_end_date | character | Post-season end date. |
| last_date1st_half | character | Last date of the first half. |
| all_star_date | character | All-Star Game date. |
| first_date2nd_half | character | First date of the second half. |
| pre_season_end_date | character | Pre-season end date. |
| spring_start_date | character | Spring training start date. |
| spring_end_date | character | Spring training end date. |

## Examples

``` r
# \donttest{
 try(mlb_seasons_all(sport_id = 1))
#> ── MLB Seasons - All Seasons data from MLB.com ────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 04:40:27 UTC
#> # A tibble: 151 × 21
#>    season_id has_wildcard pre_season_start_date season_start_date
#>    <chr>     <lgl>        <chr>                 <chr>            
#>  1 1876      FALSE        1876-01-01            1876-04-22       
#>  2 1877      FALSE        1877-01-01            1877-04-30       
#>  3 1878      FALSE        1878-01-01            1878-05-01       
#>  4 1879      FALSE        1879-01-01            1879-05-01       
#>  5 1880      FALSE        1880-01-01            1880-05-01       
#>  6 1881      FALSE        1881-01-01            1881-04-30       
#>  7 1882      FALSE        1882-01-01            1882-05-01       
#>  8 1883      FALSE        1883-01-01            1883-05-01       
#>  9 1884      FALSE        1884-01-01            1884-05-01       
#> 10 1885      FALSE        1885-01-01            1885-04-30       
#> # ℹ 141 more rows
#> # ℹ 17 more variables: regular_season_start_date <chr>,
#> #   regular_season_end_date <chr>, season_end_date <chr>,
#> #   offseason_start_date <chr>, off_season_end_date <chr>,
#> #   season_level_gameday_type <chr>, game_level_gameday_type <chr>,
#> #   qualifier_plate_appearances <dbl>, qualifier_outs_pitched <int>,
#> #   post_season_start_date <chr>, post_season_end_date <chr>, …
# }
```

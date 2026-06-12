# **MLB Sport IDs Information**

**MLB Sport IDs Information**

## Usage

``` r
mlb_sports_info(sport_id = 1)
```

## Arguments

- sport_id:

  The sport_id to return information for.

## Value

Returns a tibble with the following columns

|                    |           |                                       |
|--------------------|-----------|---------------------------------------|
| col_name           | types     | description                           |
| sport_id           | integer   | MLBAM sport (level) identifier.       |
| sport_code         | character | Short sport code (e.g. 'mlb', 'aaa'). |
| sport_link         | character | API link to the sport resource.       |
| sport_name         | character | Full sport/level name.                |
| sport_abbreviation | character | Sport abbreviation (e.g. 'MLB').      |
| sort_order         | integer   | Display sort order for the sport.     |
| active_status      | logical   | Whether the sport/level is active.    |

## Examples

``` r
# \donttest{
  try(mlb_sports_info(sport_id = 1))
#> ── MLB Sports Info data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:25:09 UTC
#> # A tibble: 1 × 7
#>   sport_id sport_code sport_link       sport_name     sport_abbreviation
#>      <int> <chr>      <chr>            <chr>          <chr>             
#> 1        1 mlb        /api/v1/sports/1 Major League … MLB               
#> # ℹ 2 more variables: sort_order <int>, active_status <lgl>
# }
```

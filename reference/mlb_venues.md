# **Find MLB Venues**

**Find MLB Venues**

## Usage

``` r
mlb_venues(venue_ids = NULL, sport_ids = NULL, season = NULL)
```

## Arguments

- venue_ids:

  Venue directorial information based venue_id.

- sport_ids:

  The sport_id(s) for which to return venue directorial information.

- season:

  Year for which to return venue directorial information for a given
  season.

## Value

Returns a tibble with the following columns:

|            |           |                                     |
|------------|-----------|-------------------------------------|
| col_name   | types     | description                         |
| venue_id   | integer   | Venue MLBAM ID.                     |
| venue_name | character | Venue name.                         |
| venue_link | character | API link to the venue.              |
| active     | logical   | Whether the venue is active.        |
| season     | character | Season the venue record applies to. |

## Examples

``` r
# \donttest{
  try(mlb_venues())
#> ── MLB Venues data from MLB.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:57:29 UTC
#> # A tibble: 1,647 × 5
#>    venue_id venue_name                venue_link          active season
#>       <int> <chr>                     <chr>               <lgl>  <chr> 
#>  1     2857 Veterans Memorial Stadium /api/v1/venues/2857 TRUE   2026  
#>  2     7253 ONT Field                 /api/v1/venues/7253 TRUE   2026  
#>  3     7258 Sun Marine Stadium        /api/v1/venues/7258 TRUE   2026  
#>  4     7260 Coloso del Pacifico       /api/v1/venues/7260 TRUE   2026  
#>  5     7261 Petco Park Events         /api/v1/venues/7261 TRUE   2026  
#>  6     7262 Esmond Athletic Field     /api/v1/venues/7262 FALSE  2026  
#>  7     3089 Kyocera Dome              /api/v1/venues/3089 TRUE   2026  
#>  8     4429 Hillsboro Ballpark        /api/v1/venues/4429 FALSE  2025  
#>  9     2835 Valley Strong Ballpark    /api/v1/venues/2835 TRUE   2026  
#> 10     6137 NEW VENUE TBD             /api/v1/venues/6137 TRUE   2026  
#> # ℹ 1,637 more rows
  try(mlb_venues(venue_ids = 4781))
#> ── MLB Venues data from MLB.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:57:29 UTC
#> # A tibble: 1 × 5
#>   venue_id venue_name venue_link          active season
#>      <int> <chr>      <chr>               <lgl>  <chr> 
#> 1     4781 Bush Field /api/v1/venues/4781 FALSE  1995  
  try(mlb_venues(sport_ids = 1))
#> ── MLB Venues data from MLB.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:57:29 UTC
#> # A tibble: 59 × 5
#>    venue_id venue_name                     venue_link      active season
#>       <int> <chr>                          <chr>           <lgl>  <chr> 
#>  1        1 Angel Stadium                  /api/v1/venues… TRUE   2026  
#>  2        2 Oriole Park at Camden Yards    /api/v1/venues… TRUE   2026  
#>  3        3 Fenway Park                    /api/v1/venues… TRUE   2026  
#>  4     5380 CoolToday Park                 /api/v1/venues… TRUE   2026  
#>  5        4 Rate Field                     /api/v1/venues… TRUE   2026  
#>  6        5 Progressive Field              /api/v1/venues… TRUE   2026  
#>  7        7 Kauffman Stadium               /api/v1/venues… TRUE   2026  
#>  8     5000 CACTI Park of the Palm Beaches /api/v1/venues… TRUE   2026  
#>  9       12 Tropicana Field                /api/v1/venues… TRUE   2026  
#> 10     2700 BayCare Ballpark               /api/v1/venues… TRUE   2026  
#> # ℹ 49 more rows
# }
```

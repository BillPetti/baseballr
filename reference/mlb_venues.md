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
#> ── MLB Venues data from MLB.com ───────────────────── baseballr 2.1.0 ──
#> ℹ Data updated: 2026-11-24 18:40:30 UTC
#> # A tibble: 1,671 × 5
#>    venue_id venue_name                   venue_link        active season
#>       <int> <chr>                        <chr>             <lgl>  <chr> 
#>  1        1 Angel Stadium                /api/v1/venues/1  TRUE   2026  
#>  2        2 Oriole Park at Camden Yards  /api/v1/venues/2  TRUE   2026  
#>  3        3 Fenway Park                  /api/v1/venues/3  TRUE   2026  
#>  4        4 Rate Field                   /api/v1/venues/4  TRUE   2026  
#>  5        5 Progressive Field            /api/v1/venues/5  TRUE   2026  
#>  6        6 Tiger Stadium                /api/v1/venues/6  FALSE  1999  
#>  7        7 Kauffman Stadium             /api/v1/venues/7  TRUE   2026  
#>  8        8 Hubert H. Humphrey Metrodome /api/v1/venues/8  FALSE  2009  
#>  9        9 Yankee Stadium I             /api/v1/venues/9  FALSE  2008  
#> 10       10 Oakland Coliseum             /api/v1/venues/10 TRUE   2024  
#> # ℹ 1,661 more rows
  try(mlb_venues(venue_ids = 4781))
#> ── MLB Venues data from MLB.com ───────────────────── baseballr 2.1.0 ──
#> ℹ Data updated: 2026-11-24 18:40:30 UTC
#> # A tibble: 1 × 5
#>   venue_id venue_name venue_link          active season
#>      <int> <chr>      <chr>               <lgl>  <chr> 
#> 1     4781 Bush Field /api/v1/venues/4781 FALSE  1995  
  try(mlb_venues(sport_ids = 1))
#> ── MLB Venues data from MLB.com ───────────────────── baseballr 2.1.0 ──
#> ℹ Data updated: 2026-11-24 18:40:31 UTC
#> # A tibble: 59 × 5
#>    venue_id venue_name                     venue_link      active season
#>       <int> <chr>                          <chr>           <lgl>  <chr> 
#>  1        1 Angel Stadium                  /api/v1/venues… TRUE   2026  
#>  2        2 Oriole Park at Camden Yards    /api/v1/venues… TRUE   2026  
#>  3        3 Fenway Park                    /api/v1/venues… TRUE   2026  
#>  4        4 Rate Field                     /api/v1/venues… TRUE   2026  
#>  5     5380 CoolToday Park                 /api/v1/venues… TRUE   2026  
#>  6        5 Progressive Field              /api/v1/venues… TRUE   2026  
#>  7        7 Kauffman Stadium               /api/v1/venues… TRUE   2026  
#>  8     5000 CACTI Park of the Palm Beaches /api/v1/venues… TRUE   2026  
#>  9     2700 BayCare Ballpark               /api/v1/venues… TRUE   2026  
#> 10       12 Tropicana Field                /api/v1/venues… TRUE   2026  
#> # ℹ 49 more rows
# }
```

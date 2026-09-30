# **Load MLB park dimensions by season from the SportsDataverse data repo**

One row per MLB venue per season from 2001 on (regular-season,
spring-training, neutral and international sites), with the fence
distances, capacity, surface, roof, orientation and location **as of
that season**, from the MLB Stats API `venues` endpoint. Published as
one season-less file to the `mlb_parks` release tag on the
[sportsdataverse-data
releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
by
[sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).

The API lags or misses some fence moves; cited corrections (Camden
Yards, Petco Park, T-Mobile Park, Comerica Park, and 2022 at Rate Field
and Progressive Field) are applied and described in `notes`.

## Usage

``` r
load_mlb_park_dimensions(..., dbConnection = NULL, tablename = NULL)
```

## Arguments

- ...:

  Additional arguments passed to an underlying function that writes the
  data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A `baseballr_data` tibble:

|  |  |  |
|----|----|----|
| col_name | types | description |
| league | character | League key (`mlb`). |
| season | integer | Season (calendar year, 2001 on). |
| venue_id | character | MLB Stats API venue id (`venue.id` in MLB game feeds and schedules). |
| venue_name | character | Venue name as of that season (e.g. PacBell Park 2001-03, Oracle Park 2019-). |
| retro_park_id | character | Retrosheet park id (e.g. `BOS07`); `NA` for most spring-training parks. |
| left_line_ft | integer | Feet from home plate to the fence at the left-field pole. |
| left_ft | integer | Feet to the fence at MLB's left-field marker. |
| left_center_ft | integer | Feet to the fence at MLB's left-centre marker. |
| center_ft | integer | Feet to the fence in straightaway centre field. |
| right_center_ft | integer | Feet to the fence at MLB's right-centre marker. |
| right_ft | integer | Feet to the fence at MLB's right-field marker. |
| right_line_ft | integer | Feet from home plate to the fence at the right-field pole. |
| capacity | integer | Seating capacity. |
| turf_type | character | `Grass` or `Artificial Turf`. |
| roof_type | character | `Open`, `Retractable` or `Dome`. |
| azimuth_deg | numeric | Degrees clockwise from north of the home plate to centre field line (Fenway 45). |
| elevation_ft | integer | Feet above sea level. |
| latitude | numeric | Latitude, decimal degrees. |
| longitude | numeric | Longitude, decimal degrees. |
| notes | character | `NA` unless a curated correction applies: what changed, why, and the citation. |

## Examples

``` r
# \donttest{
  try(load_mlb_park_dimensions())
#> ── MLB park dimensions from the SportsDataverse data repo ──────────────
#> ℹ Data updated: 2026-09-30 14:28:15 UTC
#> # A tibble: 1,503 × 20
#>    league season venue_id venue_name  retro_park_id left_line_ft left_ft
#>    <chr>   <int> <chr>    <chr>       <chr>                <int>   <int>
#>  1 mlb      2001 1        Edison Int… ANA01                  330     365
#>  2 mlb      2001 10       Network As… OAK01                  330     367
#>  3 mlb      2001 12       Tropicana … STP01                  315     370
#>  4 mlb      2001 13       The Ballpa… ARL02                  332      NA
#>  5 mlb      2001 14       SkyDome     TOR02                  328      NA
#>  6 mlb      2001 15       Bank One B… PHO01                  328     376
#>  7 mlb      2001 16       Turner Fie… ATL02                  335      NA
#>  8 mlb      2001 17       Wrigley Fi… CHI11                  355      NA
#>  9 mlb      2001 18       Cinergy Fi… CIN08                   NA      NA
#> 10 mlb      2001 19       Coors Field DEN02                  347     390
#> # ℹ 1,493 more rows
#> # ℹ 13 more variables: left_center_ft <int>, center_ft <int>,
#> #   right_center_ft <int>, right_ft <int>, right_line_ft <int>,
#> #   capacity <int>, turf_type <chr>, roof_type <chr>,
#> #   azimuth_deg <dbl>, elevation_ft <int>, latitude <dbl>,
#> #   longitude <dbl>, notes <chr>
# }
```

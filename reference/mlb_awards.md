# **MLB Awards**

**MLB Awards**

## Usage

``` r
mlb_awards()
```

## Value

Returns a tibble with the following columns

|                   |           |                                          |
|-------------------|-----------|------------------------------------------|
| col_name          | types     | description                              |
| award_id          | character | Award identifier code.                   |
| award_name        | character | Award name.                              |
| award_description | character | Award description.                       |
| sort_order        | integer   | Display sort order for the award.        |
| active            | logical   | Whether the award is currently active.   |
| notes             | character | Additional notes about the award.        |
| sport_id          | integer   | MLB sport ID associated with the award.  |
| sport_link        | character | MLB Stats API relative sport link.       |
| league_id         | integer   | MLB league ID associated with the award. |
| league_link       | character | MLB Stats API relative league link.      |

## Examples

``` r
# \donttest{
  try(mlb_awards())
#> ── MLB Awards data from MLB.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 01:56:54 UTC
#> # A tibble: 682 × 10
#>    award_id       award_name   award_description sort_order active notes
#>    <chr>          <chr>        <chr>                  <int> <lgl>  <chr>
#>  1 MLBHOF         Hall Of Fame Member of Hall o…          1 TRUE   NA   
#>  2 RETIREDUNI_108 Uniform num… NA                         4 TRUE   NA   
#>  3 RETIREDUNI_109 Uniform num… NA                         4 TRUE   NA   
#>  4 RETIREDUNI_110 Uniform num… NA                         4 TRUE   NA   
#>  5 RETIREDUNI_111 Uniform num… NA                         4 TRUE   NA   
#>  6 RETIREDUNI_112 Uniform num… NA                         4 TRUE   NA   
#>  7 RETIREDUNI_113 Uniform num… NA                         4 TRUE   NA   
#>  8 RETIREDUNI_114 Uniform num… NA                         4 TRUE   NA   
#>  9 RETIREDUNI_115 Uniform num… NA                         4 TRUE   NA   
#> 10 RETIREDUNI_116 Uniform num… NA                         4 TRUE   NA   
#> # ℹ 672 more rows
#> # ℹ 4 more variables: sport_id <int>, sport_link <chr>,
#> #   league_id <int>, league_link <chr>
# }
```

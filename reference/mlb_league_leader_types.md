# **MLB League Leader Types**

**MLB League Leader Types**

## Usage

``` r
mlb_league_leader_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| leader_type | character | League leader category display name (e.g. 'assists', 'shutouts'). |

## Examples

``` r
# \donttest{
  try(mlb_league_leader_types())
#> ── MLB League Leader Types data from MLB.com ──────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:23:59 UTC
#> # A tibble: 70 × 1
#>    leader_type           
#>    <chr>                 
#>  1 assists               
#>  2 shutouts              
#>  3 homeRuns              
#>  4 sacrificeBunts        
#>  5 sacrificeFlies        
#>  6 runs                  
#>  7 groundoutToFlyoutRatio
#>  8 stolenBases           
#>  9 battingAverage        
#> 10 groundOuts            
#> # ℹ 60 more rows
# }
```

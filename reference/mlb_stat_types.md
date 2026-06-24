# **MLB Stat Types**

**MLB Stat Types**

## Usage

``` r
mlb_stat_types()
```

## Value

Returns a tibble with the following columns

|                |           |                                           |
|----------------|-----------|-------------------------------------------|
| col_name       | types     | description                               |
| stat_type_name | character | Stat type display name (e.g., projected). |

## Examples

``` r
# \donttest{
  try(mlb_stat_types())
#> ── MLB Stat Types data from MLB.com ───────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-24 02:06:34 UTC
#> # A tibble: 61 × 1
#>    stat_type_name     
#>    <chr>              
#>  1 projected          
#>  2 projectedRos       
#>  3 yearByYear         
#>  4 yearByYearAdvanced 
#>  5 yearByYearPlayoffs 
#>  6 season             
#>  7 standard           
#>  8 advanced           
#>  9 career             
#> 10 careerRegularSeason
#> # ℹ 51 more rows
# }
```

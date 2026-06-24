# **MLB Stat Groups**

**MLB Stat Groups**

## Usage

``` r
mlb_stat_groups()
```

## Value

Returns a tibble with the following columns

|                 |           |                                          |
|-----------------|-----------|------------------------------------------|
| col_name        | types     | description                              |
| stat_group_name | character | Stat group display name (e.g., hitting). |

## Examples

``` r
# \donttest{
  try(mlb_stat_groups())
#> ── MLB Stat Groups data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-24 02:06:34 UTC
#> # A tibble: 8 × 1
#>   stat_group_name
#>   <chr>          
#> 1 hitting        
#> 2 pitching       
#> 3 fielding       
#> 4 catching       
#> 5 running        
#> 6 game           
#> 7 team           
#> 8 streak         
# }
```

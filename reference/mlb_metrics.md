# **MLB Metrics**

**MLB Metrics**

## Usage

``` r
mlb_metrics()
```

## Value

Returns a tibble with the following columns

|             |           |                                                     |
|-------------|-----------|-----------------------------------------------------|
| col_name    | types     | description                                         |
| metric_name | character | Metric name (e.g. 'releaseSpinRate').               |
| metric_id   | integer   | Numeric metric identifier.                          |
| stat_group  | character | Stat group the metric belongs to (e.g. 'pitching'). |
| metric_unit | character | Unit of measure for the metric (e.g. 'RPM').        |

## Examples

``` r
# \donttest{
  try(mlb_metrics())
#> ── MLB Metrics data from MLB.com ──────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-09 20:43:45 UTC
#> # A tibble: 22 × 4
#>    metric_name        metric_id stat_group        metric_unit
#>    <chr>                  <int> <chr>             <chr>      
#>  1 ""                         0 NA                NA         
#>  2 "releaseSpinRate"       1000 pitching          RPM        
#>  3 "releaseExtension"      1001 pitching          FT         
#>  4 "releaseSpeed"          1002 pitching          MPH        
#>  5 "effectiveSpeed"        1028 pitching          MPH        
#>  6 "launchSpeed"           1003 hitting, pitching MPH        
#>  7 "launchAngle"           1005 hitting, pitching DEG        
#>  8 "generatedSpeed"        1044 hitting, pitching MPH        
#>  9 "maxHeight"             1039 hitting           FT         
#> 10 "travelTime"            1049 hitting           SEC        
#> # ℹ 12 more rows
# }
```

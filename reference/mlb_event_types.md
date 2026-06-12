# **MLB Event Types**

**MLB Event Types**

## Usage

``` r
mlb_event_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| plate_appearance | logical | Whether the event counts as a plate appearance. |
| hit | logical | Whether the event is a hit. |
| event_code | character | Event type code. |
| base_running_event | logical | Whether the event is a base-running event. |
| event_description | character | Human-readable event description. |

## Examples

``` r
# \donttest{
  try(mlb_event_types())
#> ── MLB Event Types data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 13:46:17 UTC
#> # A tibble: 74 × 5
#>    plate_appearance hit   event_code       base_running_event
#>    <lgl>            <lgl> <chr>            <lgl>             
#>  1 FALSE            FALSE pickoff_1b       TRUE              
#>  2 FALSE            FALSE pickoff_2b       TRUE              
#>  3 FALSE            FALSE pickoff_3b       TRUE              
#>  4 FALSE            FALSE pitcher_step_off FALSE             
#>  5 FALSE            FALSE pickoff_error_1b TRUE              
#>  6 FALSE            FALSE pickoff_error_2b TRUE              
#>  7 FALSE            FALSE pickoff_error_3b TRUE              
#>  8 FALSE            FALSE batter_timeout   FALSE             
#>  9 FALSE            FALSE mound_visit      FALSE             
#> 10 FALSE            FALSE no_pitch         FALSE             
#> # ℹ 64 more rows
#> # ℹ 1 more variable: event_description <chr>
# }
```

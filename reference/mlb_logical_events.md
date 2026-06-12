# **MLB Logical Events**

**MLB Logical Events**

## Usage

``` r
mlb_logical_events()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_code | character | Logical event code used by the MLB Gameday feed (e.g. 'newBatter'). |

## Examples

``` r
# \donttest{
  try(mlb_logical_events())
#> ── MLB Logical Events data from MLB.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:24:55 UTC
#> # A tibble: 73 × 1
#>    event_code                     
#>    <chr>                          
#>  1 sceneStateUpdate               
#>  2 newBatter                      
#>  3 newLeftHandedBatter            
#>  4 newLeftHandedBatterWithPitches 
#>  5 newRightHandedBatter           
#>  6 newRightHandedBatterWithPitches
#>  7 newRightHandedHit              
#>  8 newLeftHandedHit               
#>  9 batterSwitchedToLeftHanded     
#> 10 batterSwitchedToRightHanded    
#> # ℹ 63 more rows
# }
```

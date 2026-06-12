# **MLB Pitch Types**

**MLB Pitch Types**

## Usage

``` r
mlb_pitch_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| pitch_type_code | character | Short code identifying the pitch type (e.g. 'FA'). |
| pitch_type_description | character | Full name of the pitch type (e.g. 'Fastball'). |

## Examples

``` r
# \donttest{
  try(mlb_pitch_types())
#> ── MLB Pitch Types data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:25:00 UTC
#> # A tibble: 24 × 2
#>    pitch_type_code pitch_type_description
#>    <chr>           <chr>                 
#>  1 FA              Fastball              
#>  2 FF              Four-seam FB          
#>  3 FT              Two-seam FB           
#>  4 FC              Cutter                
#>  5 FS              Splitter              
#>  6 FO              Forkball              
#>  7 SI              Sinker                
#>  8 ST              Sweeper               
#>  9 SL              Slider                
#> 10 CU              Curveball             
#> # ℹ 14 more rows
# }
```

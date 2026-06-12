# **MLB Review Reasons**

**MLB Review Reasons**

## Usage

``` r
mlb_review_reasons()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| review_reason_code | character | Short code for the replay review reason. |
| review_reason_description | character | Description of the review reason (e.g. 'Tag play'). |

## Examples

``` r
# \donttest{
  try(mlb_review_reasons())
#> ── MLB Review Reasons data from MLB.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:42:47 UTC
#> # A tibble: 27 × 2
#>    review_reason_code review_reason_description
#>    <chr>              <chr>                    
#>  1 A                  Tag play                 
#>  2 F                  Close play at 1st        
#>  3 C                  Force play               
#>  4 H                  Home run                 
#>  5 O                  Fair/foul in outfield    
#>  6 D                  Catch/drop in outfield   
#>  7 T                  Trap play in outfield    
#>  8 J                  Pitch result             
#>  9 I                  Hit by pitch             
#> 10 P                  Home-plate collision     
#> # ℹ 17 more rows
# }
```

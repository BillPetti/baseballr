# **MLB Situation Codes**

**MLB Situation Codes**

## Usage

``` r
mlb_situation_codes()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| situation_code | character | Code identifying the game situation. |
| sort_order | integer | Display sort order for the situation code. |
| navigation_menu | character | Navigation menu grouping (e.g. 'Game'). |
| situation_code_description | character | Description of the situation (e.g. 'Home Games'). |
| team | logical | Whether the situation applies to team stats. |
| batting | logical | Whether the situation applies to batting stats. |
| fielding | logical | Whether the situation applies to fielding stats. |
| pitching | logical | Whether the situation applies to pitching stats. |

## Examples

``` r
# \donttest{
  try(mlb_situation_codes())
#> ── MLB Situation Codes data from MLB.com ──────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 01:57:28 UTC
#> # A tibble: 602 × 8
#>    situation_code sort_order navigation_menu situation_code_description
#>    <chr>               <int> <chr>           <chr>                     
#>  1 h                       1 Game            Home Games                
#>  2 a                       2 Game            Away Games                
#>  3 d                       3 Game            Day Games                 
#>  4 n                       4 Game            Night Games               
#>  5 g                       5 Game            On Grass                  
#>  6 t                       6 Game            On Turf                   
#>  7 2                      20 Month           February                  
#>  8 3                      21 Month           March                     
#>  9 4                      22 Month           April                     
#> 10 5                      23 Month           May                       
#> # ℹ 592 more rows
#> # ℹ 4 more variables: team <lgl>, batting <lgl>, fielding <lgl>,
#> #   pitching <lgl>
# }
```

# **MLB Positions**

**MLB Positions**

## Usage

``` r
mlb_positions()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| position_short_name | character | Short position name (e.g. 'Pitcher'). |
| position_full_name | character | Full position name. |
| position_abbreviation | character | Position abbreviation (e.g. 'P', 'SS'). |
| position_code | character | Numeric scorekeeping position code. |
| position_type | character | Position category (e.g. 'Pitcher', 'Infielder'). |
| position_formal_name | character | Formal position name. |
| position_display_name | character | Display name for the position. |
| outfield | logical | Whether the position is an outfield position. |
| game_position | logical | Whether it is an on-field game position. |
| pitcher | logical | Whether the position is a pitcher. |
| fielder | logical | Whether the position is a fielder. |

## Examples

``` r
# \donttest{
  try(mlb_positions())
#> ── MLB Positions data from MLB.com ────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:16:03 UTC
#> # A tibble: 37 × 11
#>    position_short_name position_full_name position_abbreviation
#>    <chr>               <chr>              <chr>                
#>  1 Pitcher             Pitcher            P                    
#>  2 Catcher             Catcher            C                    
#>  3 1st Base            First Base         1B                   
#>  4 2nd Base            Second Base        2B                   
#>  5 3rd Base            Third Base         3B                   
#>  6 Shortstop           Shortstop          SS                   
#>  7 Left Field          Outfielder         LF                   
#>  8 Center Field        Outfielder         CF                   
#>  9 Right Field         Outfielder         RF                   
#> 10 Designated Hitter   Designated Hitter  DH                   
#> # ℹ 27 more rows
#> # ℹ 8 more variables: position_code <chr>, position_type <chr>,
#> #   position_formal_name <chr>, position_display_name <chr>,
#> #   outfield <lgl>, game_position <lgl>, pitcher <lgl>, fielder <lgl>
# }
```

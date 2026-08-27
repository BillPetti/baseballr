# **MLB Game Types**

**MLB Game Types**

## Usage

``` r
mlb_game_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_type_id | character | Single-letter game type code (e.g. 'S', 'R', 'P', 'W'). |
| game_type_description | character | Game type description (e.g. 'Spring Training', 'Regular Season'). |

## Examples

``` r
# \donttest{
  try(mlb_game_types())
#> ── MLB Game Types data from MLB.com ───────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-08-27 10:43:48 UTC
#> # A tibble: 11 × 2
#>    game_type_id game_type_description     
#>    <chr>        <chr>                     
#>  1 S            Spring Training           
#>  2 R            Regular Season            
#>  3 F            Wild Card                 
#>  4 D            Division Series           
#>  5 L            League Championship Series
#>  6 W            World Series              
#>  7 C            Championship              
#>  8 P            Postseason                
#>  9 A            All-Star Game             
#> 10 I            Intrasquad                
#> 11 E            Exhibition                
# }
```

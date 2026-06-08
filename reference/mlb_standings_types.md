# **MLB Standings Types**

**MLB Standings Types**

## Usage

``` r
mlb_standings_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| standings_type_name | character | Standings type identifier (e.g., wildCard). |
| standings_type_description | character | Human-readable description of the type. |

## Examples

``` r
# \donttest{
  try(mlb_standings_types())
#> ── MLB Standings Types data from MLB.com ──────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 04:40:29 UTC
#> # A tibble: 13 × 2
#>    standings_type_name standings_type_description                       
#>    <chr>               <chr>                                            
#>  1 regularSeason       Regular Season Standings                         
#>  2 wildCard            Wild card standings                              
#>  3 divisionLeaders     Division Leader standings                        
#>  4 wildCardWithLeaders Wild card standings with Division Leaders        
#>  5 firstHalf           First half standings.  Only valid for leagues wi…
#>  6 secondHalf          Second half standings. Only valid for leagues wi…
#>  7 springTraining      Spring Training Standings                        
#>  8 postseason          Postseason Standings                             
#>  9 byDivision          Standings by Division                            
#> 10 byConference        Standings by Conference                          
#> 11 byLeague            Standings by League                              
#> 12 byOrganization      Standing by Organization                         
#> 13 currentHalf         Current Half Standings. Returns standings in the…
# }
```

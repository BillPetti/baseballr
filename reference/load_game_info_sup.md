# **Download a data frame of supplemental data about MLB games since 2008.**

**Download a data frame of supplemental data about MLB games since
2008.**

## Usage

``` r
load_game_info_sup()
```

## Value

Function returns a tibble with various columns, including:

- game_pk

- game_date

- venue id

- attendance

- game temperature

- wind speed

- direction

- start time

- end time

## Examples

``` r
# \donttest{
  try(load_game_info_sup())
#> ── Supplementary MLB Game Info data from baseballr-data repository ─────
#> ℹ Data updated: 2026-06-08 04:39:36 UTC
#> # A tibble: 41,946 × 18
#>    game_date  game_pk venue_name      venue_id temperature other_weather
#>    <IDate>      <int> <chr>              <int>       <int> <chr>        
#>  1 2022-10-05  663451 Oriole Park at…        2          59 Overcast     
#>  2 2022-10-05  663272 Oriole Park at…        2          59 Overcast     
#>  3 2022-10-05  662421 Oakland Colise…       10          69 Sunny        
#>  4 2022-10-05  662304 PNC Park              31          72 Sunny        
#>  5 2022-10-05  661996 Globe Life Fie…     5325          86 Sunny        
#>  6 2022-10-05  663131 Great American…     2602          76 Sunny        
#>  7 2022-10-05  663095 Progressive Fi…        5          68 Sunny        
#>  8 2022-10-05  662488 Citi Field          3289          59 Overcast     
#>  9 2022-10-05  662195 T-Mobile Park        680          57 Cloudy       
#> 10 2022-10-05  662238 Petco Park          2680          68 Cloudy       
#> # ℹ 41,936 more rows
#> # ℹ 12 more variables: wind <chr>, attendance <chr>, start_time <chr>,
#> #   elapsed_time <chr>, game_id <chr>, game_type <chr>,
#> #   home_sport_code <chr>, official_scorer <chr>, date <chr>,
#> #   status_ind <chr>, home_league_id <int>, gameday_sw <chr>
# }
```

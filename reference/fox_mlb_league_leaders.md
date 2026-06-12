# **Get Fox Sports MLB statistical leaders**

**Get Fox Sports MLB statistical leaders**

## Usage

``` r
fox_mlb_league_leaders(category = "batting", who = "player", page = 0)
```

## Arguments

- category:

  Stat category (default `"batting"`).

- who:

  `"player"` or `"team"` (default `"player"`).

- page:

  0-based page index (default `0`).

## Value

A `baseballr_data` tibble of leaderboard rows (`entity_id` + stat
columns).

## Examples

``` r
 try(fox_mlb_league_leaders("batting")) 
#> ── Fox Sports MLB league_leaders ──────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:23:14 UTC
#> # A tibble: 100 × 7
#>    players v2           g     entity_id pa    ab    h    
#>    <chr>   <chr>        <chr> <chr>     <chr> <chr> <chr>
#>  1 1       J. Ramírez   70    5379      NA    NA    NA   
#>  2 2       P. Alonso    70    8988      NA    NA    NA   
#>  3 3       R. Arozarena 70    10191     NA    NA    NA   
#>  4 4       J. Rodríguez 70    11137     NA    NA    NA   
#>  5 5       C. Young     70    13642     NA    NA    NA   
#>  6 6       C. Walker    69    6340      NA    NA    NA   
#>  7 7       R. Devers    69    8041      NA    NA    NA   
#>  8 8       T. Ward      69    8072      NA    NA    NA   
#>  9 9       Y. Alvarez   69    9387      NA    NA    NA   
#> 10 10      J. Adell     69    9396      NA    NA    NA   
#> # ℹ 90 more rows
```

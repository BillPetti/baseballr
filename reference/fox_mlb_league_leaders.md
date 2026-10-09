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
#> ℹ Data updated: 2026-10-09 04:40:39 UTC
#> # A tibble: 100 × 7
#>    players v2           g     entity_id pa    ab    h    
#>    <chr>   <chr>        <chr> <chr>     <chr> <chr> <chr>
#>  1 1       M. Olson     7     5666      NA    NA    NA   
#>  2 2       O. Albies    7     7214      NA    NA    NA   
#>  3 3       M. Dubón     7     8279      NA    NA    NA   
#>  4 4       A. Riley     7     8616      NA    NA    NA   
#>  5 5       R. Acuña Jr. 7     8767      NA    NA    NA   
#>  6 6       M. Harris II 7     11699     NA    NA    NA   
#>  7 7       D. Baldwin   7     13108     NA    NA    NA   
#>  8 8       M. Machado   6     5147      NA    NA    NA   
#>  9 9       X. Bogaerts  6     5359      NA    NA    NA   
#> 10 10      F. Tatis Jr. 6     8761      NA    NA    NA   
#> # ℹ 90 more rows
```

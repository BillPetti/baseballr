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
#> ℹ Data updated: 2026-06-24 02:05:38 UTC
#> # A tibble: 100 × 7
#>    players v2           g     entity_id pa    ab    h    
#>    <chr>   <chr>        <chr> <chr>     <chr> <chr> <chr>
#>  1 1       C. Walker    80    6340      NA    NA    NA   
#>  2 2       P. Alonso    80    8988      NA    NA    NA   
#>  3 3       Y. Alvarez   80    9387      NA    NA    NA   
#>  4 4       J. Adell     80    9396      NA    NA    NA   
#>  5 5       C. Young     80    13642     NA    NA    NA   
#>  6 6       T. Ward      79    8072      NA    NA    NA   
#>  7 7       B. Reynolds  79    10496     NA    NA    NA   
#>  8 8       J. Rodríguez 79    11137     NA    NA    NA   
#>  9 9       X. Edwards   79    11608     NA    NA    NA   
#> 10 10      G. Henderson 79    11726     NA    NA    NA   
#> # ℹ 90 more rows
```

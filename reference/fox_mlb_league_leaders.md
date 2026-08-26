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
#> ℹ Data updated: 2026-08-26 19:18:23 UTC
#> # A tibble: 100 × 7
#>    players v2                g     entity_id pa    ab    h    
#>    <chr>   <chr>             <chr> <chr>     <chr> <chr> <chr>
#>  1 1       B. Reynolds       134   10496     NA    NA    NA   
#>  2 2       B. Harper         133   5349      NA    NA    NA   
#>  3 3       X. Edwards        133   11608     NA    NA    NA   
#>  4 4       P. Crow-Armstrong 133   11825     NA    NA    NA   
#>  5 5       C. Young          133   13642     NA    NA    NA   
#>  6 6       S. Stewart        133   14786     NA    NA    NA   
#>  7 7       M. Olson          132   5666      NA    NA    NA   
#>  8 8       O. Albies         132   7214      NA    NA    NA   
#>  9 9       R. Devers         132   8041      NA    NA    NA   
#> 10 10      P. Alonso         132   8988      NA    NA    NA   
#> # ℹ 90 more rows
```

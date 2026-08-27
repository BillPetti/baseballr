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
#> ℹ Data updated: 2026-08-27 10:42:02 UTC
#> # A tibble: 100 × 7
#>    players v2                g     entity_id pa    ab    h    
#>    <chr>   <chr>             <chr> <chr>     <chr> <chr> <chr>
#>  1 1       B. Reynolds       135   10496     NA    NA    NA   
#>  2 2       B. Harper         134   5349      NA    NA    NA   
#>  3 3       X. Edwards        134   11608     NA    NA    NA   
#>  4 4       P. Crow-Armstrong 134   11825     NA    NA    NA   
#>  5 5       S. Stewart        134   14786     NA    NA    NA   
#>  6 6       M. Olson          133   5666      NA    NA    NA   
#>  7 7       O. Albies         133   7214      NA    NA    NA   
#>  8 8       R. Devers         133   8041      NA    NA    NA   
#>  9 9       P. Alonso         133   8988      NA    NA    NA   
#> 10 10      I. Herrera        133   11292     NA    NA    NA   
#> # ℹ 90 more rows
```

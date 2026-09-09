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
#> ℹ Data updated: 2026-09-09 22:12:16 UTC
#> # A tibble: 100 × 7
#>    players v2                g     entity_id pa    ab    h    
#>    <chr>   <chr>             <chr> <chr>     <chr> <chr> <chr>
#>  1 1       R. Devers         146   8041      NA    NA    NA   
#>  2 2       P. Alonso         146   8988      NA    NA    NA   
#>  3 3       P. Crow-Armstrong 146   11825     NA    NA    NA   
#>  4 4       B. Harper         145   5349      NA    NA    NA   
#>  5 5       M. Olson          145   5666      NA    NA    NA   
#>  6 6       O. Albies         145   7214      NA    NA    NA   
#>  7 7       B. Reynolds       145   10496     NA    NA    NA   
#>  8 8       I. Herrera        145   11292     NA    NA    NA   
#>  9 9       G. Henderson      145   11726     NA    NA    NA   
#> 10 10      A. Burleson       145   12072     NA    NA    NA   
#> # ℹ 90 more rows
```

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
#> ℹ Data updated: 2026-09-27 20:53:49 UTC
#> # A tibble: 100 × 7
#>    players v2                g     entity_id pa    ab    h    
#>    <chr>   <chr>             <chr> <chr>     <chr> <chr> <chr>
#>  1 1       B. Harper         161   5349      NA    NA    NA   
#>  2 2       M. Olson          161   5666      NA    NA    NA   
#>  3 3       O. Albies         161   7214      NA    NA    NA   
#>  4 4       P. Alonso         161   8988      NA    NA    NA   
#>  5 5       B. Reynolds       161   10496     NA    NA    NA   
#>  6 6       P. Crow-Armstrong 161   11825     NA    NA    NA   
#>  7 7       J. Caminero       161   13593     NA    NA    NA   
#>  8 8       S. Stewart        161   14786     NA    NA    NA   
#>  9 9       I. Herrera        160   11292     NA    NA    NA   
#> 10 10      A. Burleson       160   12072     NA    NA    NA   
#> # ℹ 90 more rows
```

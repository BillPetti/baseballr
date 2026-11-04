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
#> ── Fox Sports MLB league_leaders ─────────────── baseballr 2.0.0.9000 ──
#> ℹ Data updated: 2026-11-04 04:38:02 UTC
#> # A tibble: 100 × 7
#>    players v2                g     entity_id pa    ab    h    
#>    <chr>   <chr>             <chr> <chr>     <chr> <chr> <chr>
#>  1 1       B. Harper         162   5349      NA    NA    NA   
#>  2 2       M. Olson          162   5666      NA    NA    NA   
#>  3 3       O. Albies         162   7214      NA    NA    NA   
#>  4 4       B. Reynolds       162   10496     NA    NA    NA   
#>  5 5       P. Crow-Armstrong 162   11825     NA    NA    NA   
#>  6 6       J. Caminero       162   13593     NA    NA    NA   
#>  7 7       S. Stewart        162   14786     NA    NA    NA   
#>  8 8       P. Alonso         161   8988      NA    NA    NA   
#>  9 9       I. Herrera        161   11292     NA    NA    NA   
#> 10 10      B. Bichette       160   9447      NA    NA    NA   
#> # ℹ 90 more rows
```

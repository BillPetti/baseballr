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
#> ℹ Data updated: 2026-09-30 14:28:02 UTC
#> # A tibble: 100 × 7
#>    players v2             g     entity_id pa    ab    h    
#>    <chr>   <chr>          <chr> <chr>     <chr> <chr> <chr>
#>  1 1       P. Goldschmidt 1     5016      NA    NA    NA   
#>  2 2       J. Altuve      1     5058      NA    NA    NA   
#>  3 3       M. Machado     1     5147      NA    NA    NA   
#>  4 4       B. Harper      1     5349      NA    NA    NA   
#>  5 5       X. Bogaerts    1     5359      NA    NA    NA   
#>  6 6       T. Story       1     5645      NA    NA    NA   
#>  7 7       D. Smith       1     5657      NA    NA    NA   
#>  8 8       M. Olson       1     5666      NA    NA    NA   
#>  9 9       J. Realmuto    1     5752      NA    NA    NA   
#> 10 10      R. Grichuk     1     5805      NA    NA    NA   
#> # ℹ 90 more rows
```

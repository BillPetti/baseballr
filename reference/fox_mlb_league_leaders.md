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
#> ℹ Data updated: 2026-07-23 17:33:39 UTC
#> # A tibble: 100 × 7
#>    players v2          g     entity_id pa    ab    h    
#>    <chr>   <chr>       <chr> <chr>     <chr> <chr> <chr>
#>  1 1       B. Harper   103   5349      NA    NA    NA   
#>  2 2       P. Alonso   103   8988      NA    NA    NA   
#>  3 3       B. Reynolds 103   10496     NA    NA    NA   
#>  4 4       X. Edwards  103   11608     NA    NA    NA   
#>  5 5       J. Wood     103   12527     NA    NA    NA   
#>  6 6       C. Young    103   13642     NA    NA    NA   
#>  7 7       R. Devers   102   8041      NA    NA    NA   
#>  8 8       T. Ward     102   8072      NA    NA    NA   
#>  9 9       Y. Alvarez  102   9387      NA    NA    NA   
#> 10 10      J. Adell    102   9396      NA    NA    NA   
#> # ℹ 90 more rows
```

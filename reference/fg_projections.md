# **Scrape FanGraphs player projections**

Retrieves FanGraphs' published player projections (Steamer, ZiPS, ATC,
THE BAT, and their variants) from the FanGraphs projections API – one
row per player with the projected stat line.

## Usage

``` r
fg_projections(
  type = "steamer",
  stats = "bat",
  team = 0,
  league = "all",
  position = "all"
)
```

## Arguments

- type:

  Projection system. One of `"steamer"`, `"zips"`, `"zipsdc"`, `"atc"`,
  `"thebat"`, `"thebatx"` (batters only), or the rest-of-season variants
  `"rsteamer"`, `"rzips"`, `"ratc"`, `"rthebat"`, `"rthebatx"`. Defaults
  to `"steamer"`.

- stats:

  Either `"bat"` (batting projections) or `"pit"` (pitching
  projections). Defaults to `"bat"`.

- team:

  MLB team filter as FanGraphs' numeric team id; `0` (default) returns
  all teams.

- league:

  League filter: `"all"` (default), `"al"`, or `"nl"`.

- position:

  Position filter (e.g. `"all"`, `"c"`, `"ss"`, `"of"`). Defaults to
  `"all"`.

## Value

A `baseballr_data` tibble with one row per player and the projection
system's stat columns (counting stats, rates, and projected WAR; column
set varies by system and by `stats`).

## Examples

``` r
# \donttest{
  try(fg_projections(type = "steamer", stats = "bat"))
#> ── FanGraphs steamer bat projections data from FanGraphs.com ───────────
#> ℹ Data updated: 2026-08-26 21:29:56 UTC
#> # A tibble: 4,186 × 82
#>    team  short_name     g    ab    pa     h   x1b   x2b   x3b    hr
#>    <chr> <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 NYY   Yankees     141.  509.  633.  144.  76.9  23.3 1.20   42.3
#>  2 KCR   Royals      140.  574.  634.  168.  99.0  36.4 4.70   28.0
#>  3 BAL   Orioles     146.  582.  666.  160.  95.7  31.7 4.27   28.1
#>  4 SEA   Mariners    138.  527.  615.  122.  61.6  21.9 0.472  37.5
#>  5 SEA   Mariners    139.  560.  618.  154.  95.1  26.7 2.35   29.8
#>  6 NYM   Mets        138.  494.  621.  135.  76.5  22.7 1.42   34.3
#>  7 LAD   Dodgers     142.  555.  658.  153.  78.0  25.2 5.86   43.6
#>  8 ATL   Braves      147.  571.  676.  163. 103.   27.1 2.37   30.8
#>  9 SDP   Padres      136.  539.  619.  148.  87.9  28.1 1.71   30.4
#> 10 NYM   Mets        139.  569.  644.  147.  91.1  29.0 0.899  26.3
#> # ℹ 4,176 more rows
#> # ℹ 72 more variables: r <dbl>, rbi <dbl>, bb <dbl>, ibb <dbl>,
#> #   so <dbl>, hbp <dbl>, sf <dbl>, sh <dbl>, gdp <lgl>, sb <dbl>,
#> #   cs <dbl>, avg <dbl>, obp <dbl>, slg <dbl>, ops <dbl>, w_oba <dbl>,
#> #   bb_percent <dbl>, k_percent <dbl>, bb_k <dbl>, iso <dbl>,
#> #   spd <dbl>, babip <dbl>, ubr <dbl>, gdp_runs <lgl>, w_rc <dbl>,
#> #   w_raa <dbl>, uzr <dbl>, w_bs_r <dbl>, base_running <dbl>, …
# }
```

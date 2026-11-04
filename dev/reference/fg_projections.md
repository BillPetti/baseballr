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
#> ℹ Data updated: 2026-11-04 05:32:00 UTC
#> # A tibble: 4,166 × 82
#>    team  short_name       g    ab    pa     h   x1b   x2b   x3b    hr
#>    <chr> <chr>        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 CHC   Cubs          145.  575.  650.  148.  82.3  27.6  5.12  33.2
#>  2 KCR   Royals        132.  531.  599.  153.  93.3  32.8  2.48  24.0
#>  3 TBR   Rays          147.  574.  647.  161.  89.6  27.5  1.08  42.5
#>  4 DET   Tigers        144.  565.  662.  156. 102.   29.4  4.23  20.8
#>  5 CIN   Reds          141.  555.  636.  150.  87.8  29.8  4.67  28.0
#>  6 HOU   Astros        129.  482.  581.  143.  83.4  26.8  1.16  32.1
#>  7 NYY   Yankees       124.  444.  550.  119.  63.5  19.5  0.94  35.2
#>  8 NYM   Mets          139.  499.  626.  135.  78.2  21.3  2.18  33.5
#>  9 BAL   Orioles       142.  554.  632.  143.  86.2  27.7  4.21  24.7
#> 10 ARI   Diamondbacks  125.  475.  542.  137.  92.8  26.9  1.43  15.4
#> # ℹ 4,156 more rows
#> # ℹ 72 more variables: r <dbl>, rbi <dbl>, bb <dbl>, ibb <dbl>,
#> #   so <dbl>, hbp <dbl>, sf <dbl>, sh <dbl>, gdp <lgl>, sb <dbl>,
#> #   cs <dbl>, avg <dbl>, obp <dbl>, slg <dbl>, ops <dbl>, w_oba <dbl>,
#> #   bb_percent <dbl>, k_percent <dbl>, bb_k <dbl>, iso <dbl>,
#> #   spd <dbl>, babip <dbl>, ubr <dbl>, gdp_runs <lgl>, w_rc <dbl>,
#> #   w_raa <dbl>, uzr <dbl>, w_bs_r <dbl>, base_running <dbl>, …
# }
```

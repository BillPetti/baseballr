# **Load pre-computed MLB model datasets from the SportsDataverse data repo**

Season-level loaders for the modeled MLB datasets published on the
[sportsdataverse-data
releases](https://github.com/sportsdataverse/sportsdataverse-data/releases),
mirroring sportsdataverse-py's `load_mlb_*` loader surface:

- `load_mlb_expected_stats()` / `load_mlb_expected_hr()` /
  `load_mlb_batter_projection()` – hitting models (xBA/xwOBA-style
  expected stats, expected home runs, and blended batter projections).

- `load_mlb_stuff_plus()` / `load_mlb_command_plus()` /
  `load_mlb_xera()` – pitching models (Stuff+, Command+, and expected
  ERA).

- `load_mlb_oaa()` / `load_mlb_catcher_framing()` – fielding models
  (outs above average and catcher framing runs).

- `load_mlb_re24_matrix()` / `load_mlb_we_table()` / `load_mlb_wpa()` –
  game-state references (run-expectancy matrix, win-expectancy table,
  and per-play win probability added).

Coverage starts in 2015 (2016 for `load_mlb_batter_projection()`) and
runs through the most recent season.

## Usage

``` r
load_mlb_expected_stats(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_expected_hr(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_batter_projection(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_stuff_plus(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_command_plus(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_xera(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_oaa(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_catcher_framing(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_re24_matrix(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_we_table(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)

load_mlb_wpa(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit seasons, or `TRUE` for every published season.

- ...:

  Additional arguments passed to the underlying database write.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A `baseballr_data` tibble (one row per player-season, or per game-state
cell for the game-state references).

## Examples

``` r
# \donttest{
  try(load_mlb_expected_stats(2024))
#> ── MLB expected stats data from the SportsDataverse data repo ──────────
#> ℹ Data updated: 2026-08-27 10:43:55 UTC
#> # A tibble: 1,777 × 7
#>    batter season    pa    ab   xwoba     xba   xslg
#>     <int>  <int> <int> <int>   <dbl>   <dbl>  <dbl>
#>  1 702690   2024     3     3 NA      0       0     
#>  2 545361   2024   736   711  0.516  0.0434  0.0911
#>  3 682722   2024     2     2  0.0144 0.00636 0.0106
#>  4 649966   2024   538   518  0.460  0.0384  0.0743
#>  5 643348   2024    39    35 NA      0       0     
#>  6 642021   2024    78    70  2.53   0.0315  0.0385
#>  7 665828   2024  1530  1497  0.329  0.0579  0.0813
#>  8 688392   2024     4     4 NA      0       0     
#>  9 669134   2024  1114  1091  0.346  0.0671  0.0972
#> 10 680869   2024  2203  2156  0.319  0.0486  0.0858
#> # ℹ 1,767 more rows
# }
```

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
#> ℹ Data updated: 2026-09-10 00:18:07 UTC
#> # A tibble: 1,777 × 9
#>    batter season    pa    ab  xwoba    xba   xslg  woba    ba
#>     <int>  <int> <int> <int>  <dbl>  <dbl>  <dbl> <dbl> <dbl>
#>  1 681044   2024     1     1 0      0      0      0     0    
#>  2 703202   2024     1     1 0      0      0      0     0    
#>  3 677341   2024     2     2 0.0363 0.0200 0.0211 0     0    
#>  4 643348   2024    15    11 0.187  0      0      0.187 0    
#>  5 676053   2024     3     3 0.3    0.333  0.333  0.3   0.333
#>  6 663979   2024     4     3 0.197  0.0280 0.0373 0.175 0    
#>  7 696145   2024     4     3 0.268  0.122  0.144  0.175 0    
#>  8 645801   2024    98    94 0.351  0.297  0.470  0.301 0.255
#>  9 687526   2024     1     1 0      0      0      0     0    
#> 10 642451   2024    33    30 0.232  0.159  0.260  0.233 0.167
#> # ℹ 1,767 more rows
# }
```

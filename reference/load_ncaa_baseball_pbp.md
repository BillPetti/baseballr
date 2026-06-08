# **Load cleaned NCAA baseball play-by-play data from the baseballr data repo**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_ncaa_baseball_pbp(
  seasons = most_recent_ncaa_baseball_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given NCAA college baseball
  seasons. (Min: 2022)

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by

- tablename:

  The name of the schedule data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_ncaa_baseball_pbp(seasons = 2021))
#> ── NCAA Play-by-Play Information from baseballr data repository ────────
#> ℹ Data updated: 2023-03-05 03:07:57 UTC
#> # A tibble: 1,760,171 × 12
#>    game_date  location    attendance inning inning_top_bot score batting
#>    <chr>      <chr>       <lgl>      <chr>  <chr>          <chr> <chr>  
#>  1 06/30/2021 Omaha, Neb. NA         1      top            0-0   Missis…
#>  2 06/30/2021 Omaha, Neb. NA         1      top            0-0   Missis…
#>  3 06/30/2021 Omaha, Neb. NA         1      top            0-0   Missis…
#>  4 06/30/2021 Omaha, Neb. NA         1      top            0-0   Missis…
#>  5 06/30/2021 Omaha, Neb. NA         1      top            0-0   Missis…
#>  6 06/30/2021 Omaha, Neb. NA         1      top            1-0   Missis…
#>  7 06/30/2021 Omaha, Neb. NA         1      top            1-0   Missis…
#>  8 06/30/2021 Omaha, Neb. NA         1      bot            1-0   Vander…
#>  9 06/30/2021 Omaha, Neb. NA         1      bot            1-0   Vander…
#> 10 06/30/2021 Omaha, Neb. NA         1      bot            1-0   Vander…
#> # ℹ 1,760,161 more rows
#> # ℹ 5 more variables: fielding <chr>, description <chr>, year <int>,
#> #   game_pbp_url <chr>, game_pbp_id <int>
# }
```

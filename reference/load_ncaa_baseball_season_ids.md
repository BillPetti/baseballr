# **Load cleaned NCAA men's college baseball season IDs from the baseballr data repo**

helper that loads multiple seasons of season IDs from the data repo
either into memory or writes it into a db using some forwarded arguments
in the dots

## Usage

``` r
load_ncaa_baseball_season_ids(..., dbConnection = NULL, tablename = NULL)
```

## Arguments

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by

- tablename:

  The name of the data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_ncaa_baseball_season_ids())
#> ── NCAA Baseball Season IDs from baseballr data repository ─────────────
#> ℹ Data updated: 2026-05-29 22:22:52 UTC
#> # A tibble: 15 × 5
#>    season    id batting_id pitching_id fielding_id
#>     <dbl> <dbl>      <dbl>       <dbl>       <dbl>
#>  1   2012 10942      10082       10083       10084
#>  2   2013 11320      10120       10121       10122
#>  3   2014 11620      10460       10461       10462
#>  4   2015 12080      10780       10781       10782
#>  5   2016 12360      10946       10947       10948
#>  6   2017 12560      11000       11001       11002
#>  7   2018 12973      11953       11954       11955
#>  8   2019 14781      14643       14644       14645
#>  9   2020 15204      14760       14761       14762
#> 10   2021 15580      14840       14841       14842
#> 11   2022 15860      14940       14941       14942
#> 12   2023 16340      15000       15001       15002
#> 13   2024 16580      15080       15081       15082
#> 14   2025 16840      15687       15688       15689
#> 15   2026 17040      15867       15868       15869
# }
```

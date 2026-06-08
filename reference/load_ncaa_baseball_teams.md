# **Load cleaned NCAA men's college baseball teams from the baseballr data repo**

helper that loads multiple seasons of teams from the data repo either
into memory or writes it into a db using some forwarded arguments in the
dots

## Usage

``` r
load_ncaa_baseball_teams(..., dbConnection = NULL, tablename = NULL)
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
  try(load_ncaa_baseball_teams())
#> ── NCAA Baseball Teams Information from baseballr data repository ──────
#> ℹ Data updated: 2026-05-30 01:50:36 UTC
#> # A tibble: 15,928 × 9
#>    team_id team_name    team_url conference_id conference division  year
#>      <dbl> <chr>        <chr>            <dbl> <chr>         <dbl> <dbl>
#>  1   26172 A&M-Corpus … /teams/…           914 Southland         1  2026
#>  2   26172 A&M-Corpus … /teams/…           914 Southland         1  2025
#>  3   26172 A&M-Corpus … /team/2…           914 Southland         1  2024
#>  4   26172 A&M-Corpus … /team/2…           914 Southland         1  2023
#>  5   26172 A&M-Corpus … /team/2…           914 Southland         1  2022
#>  6   26172 A&M-Corpus … /team/2…           914 Southland         1  2021
#>  7   26172 A&M-Corpus … /team/2…           914 Southland         1  2020
#>  8   26172 A&M-Corpus … /team/2…           914 Southland         1  2019
#>  9   26172 A&M-Corpus … /team/2…           914 Southland         1  2018
#> 10   26172 A&M-Corpus … /team/2…           914 Southland         1  2017
#> # ℹ 15,918 more rows
#> # ℹ 2 more variables: season_id <dbl>, season_team_id <dbl>
# }
```

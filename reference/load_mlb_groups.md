# **Load MLB groups (leagues and divisions) from the SportsDataverse data repo**

One row per MLB group lineage (the league, the American and National
Leagues, and their divisions), keyed by the SportsDataverse group id.
Published to the `mlb_groups` release tag on the [sportsdataverse-data
releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
by
[sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).
Season keys are the single calendar year.

## Usage

``` r
load_mlb_groups(..., dbConnection = NULL, tablename = NULL)
```

## Arguments

- ...:

  Additional arguments passed to an underlying function that writes the
  data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A `baseballr_data` tibble:

|  |  |  |
|----|----|----|
| col_name | types | description |
| league | character | League key. |
| group_id | character | SportsDataverse group id, `{league}:{slug}`; one id per lineage across renames. |
| level | character | Group level: `league`, `subdivision`, `conference` or `division`. |
| first_season | integer | First season with at least one member. |
| last_season | integer | Last season with at least one member. |
| notes | character | Lineage decisions and source caveats. |

## Examples

``` r
# \donttest{
  try(load_mlb_groups())
#> ── MLB groups from the SportsDataverse data repo ──── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-09-27 04:37:26 UTC
#> # A tibble: 17 × 6
#>    league group_id                  level first_season last_season notes
#>    <chr>  <chr>                     <chr>        <int>       <int> <chr>
#>  1 mlb    mlb:al                    conf…         1901        2026 the …
#>  2 mlb    mlb:al-central            divi…         1994        2026 new …
#>  3 mlb    mlb:al-east               divi…         1969        2026 line…
#>  4 mlb    mlb:al-west               divi…         1969        2026 1969…
#>  5 mlb    mlb:american-negro-league conf…         1929        1929 1929…
#>  6 mlb    mlb:eastern-colored-leag… conf…         1923        1928 1923…
#>  7 mlb    mlb:federal-league        conf…         1914        1915 1914…
#>  8 mlb    mlb:mlb                   leag…         1901        2026 the …
#>  9 mlb    mlb:negro-american-league conf…         1937        1948 1937…
#> 10 mlb    mlb:negro-east-west-leag… conf…         1932        1932 1932…
#> 11 mlb    mlb:negro-national-leagu… conf…         1920        1931 Negr…
#> 12 mlb    mlb:negro-national-leagu… conf…         1933        1948 Negr…
#> 13 mlb    mlb:negro-southern-league conf…         1932        1932 1932…
#> 14 mlb    mlb:nl                    conf…         1901        2026 the …
#> 15 mlb    mlb:nl-central            divi…         1994        2026 new …
#> 16 mlb    mlb:nl-east               divi…         1969        2026 1969…
#> 17 mlb    mlb:nl-west               divi…         1969        2026 1969…
# }
```

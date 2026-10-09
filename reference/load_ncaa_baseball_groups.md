# **Load NCAA baseball groups (divisions and conferences) from the SportsDataverse data repo**

One row per NCAA baseball group lineage (divisions I-III and their
conferences), keyed by the SportsDataverse group id. Published to the
`ncaa_baseball_groups` release tag on the [sportsdataverse-data
releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
by
[sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).
Season keys are the spring year (2026 = the spring 2026 season).

## Usage

``` r
load_ncaa_baseball_groups(..., dbConnection = NULL, tablename = NULL)
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
  try(load_ncaa_baseball_groups())
#> ── NCAA baseball groups from the SportsDataverse data repo ─────────────
#> ℹ Data updated: 2026-10-09 03:46:22 UTC
#> # A tibble: 107 × 6
#>    league        group_id           level first_season last_season notes
#>    <chr>         <chr>              <chr>        <int>       <int> <chr>
#>  1 ncaa_baseball ncaa_baseball:acc  conf…         2010        2026 NCAA…
#>  2 ncaa_baseball ncaa_baseball:amcc conf…         2010        2026 NCAA…
#>  3 ncaa_baseball ncaa_baseball:ame… conf…         2010        2026 NCAA…
#>  4 ncaa_baseball ncaa_baseball:ame… conf…         2010        2026 NCAA…
#>  5 ncaa_baseball ncaa_baseball:ame… conf…         2010        2026 NCAA…
#>  6 ncaa_baseball ncaa_baseball:asc  conf…         2010        2026 NCAA…
#>  7 ncaa_baseball ncaa_baseball:asun conf…         2010        2026 NCAA…
#>  8 ncaa_baseball ncaa_baseball:atl… conf…         2010        2026 NCAA…
#>  9 ncaa_baseball ncaa_baseball:atl… conf…         2019        2026 NCAA…
#> 10 ncaa_baseball ncaa_baseball:big… conf…         2010        2026 NCAA…
#> # ℹ 97 more rows
# }
```

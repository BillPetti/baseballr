# **Load NCAA baseball group names and parents by season from the SportsDataverse data repo**

One row per NCAA baseball group per season it existed, with the name,
abbreviation and parent group **as of that season** (not today's).
Published to the `ncaa_baseball_groups` release tag on the
sportsdataverse-data releases. Season keys are the spring year.

## Usage

``` r
load_ncaa_baseball_group_seasons(..., dbConnection = NULL, tablename = NULL)
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
| group_id | character | SportsDataverse group id, `{league}:{slug}`. |
| season | integer | Season. |
| level | character | Group level: `league`, `subdivision`, `conference` or `division`. |
| name | character | Group name as of that season. |
| short_name | character | Short name as of that season. |
| abbreviation | character | Abbreviation as of that season. |
| parent_group_id | character | Parent group id as of that season (division, conference, subdivision, league). |
| n_teams | integer | Member teams that season. |

## Examples

``` r
# \donttest{
  try(load_ncaa_baseball_group_seasons())
#> ── NCAA baseball group seasons from the SportsDataverse data repo ──────
#> ℹ Data updated: 2026-09-30 14:28:18 UTC
#> # A tibble: 1,709 × 9
#>    league        group_id     season level name  short_name abbreviation
#>    <chr>         <chr>         <int> <chr> <chr> <chr>      <chr>       
#>  1 ncaa_baseball ncaa_baseba…   2010 conf… Atla… ACC        ACC         
#>  2 ncaa_baseball ncaa_baseba…   2011 conf… Atla… ACC        ACC         
#>  3 ncaa_baseball ncaa_baseba…   2012 conf… Atla… ACC        ACC         
#>  4 ncaa_baseball ncaa_baseba…   2013 conf… Atla… ACC        ACC         
#>  5 ncaa_baseball ncaa_baseba…   2014 conf… Atla… ACC        ACC         
#>  6 ncaa_baseball ncaa_baseba…   2015 conf… Atla… ACC        ACC         
#>  7 ncaa_baseball ncaa_baseba…   2016 conf… Atla… ACC        ACC         
#>  8 ncaa_baseball ncaa_baseba…   2017 conf… Atla… ACC        ACC         
#>  9 ncaa_baseball ncaa_baseba…   2018 conf… Atla… ACC        ACC         
#> 10 ncaa_baseball ncaa_baseba…   2019 conf… Atla… ACC        ACC         
#> # ℹ 1,699 more rows
#> # ℹ 2 more variables: parent_group_id <chr>, n_teams <int>
# }
```

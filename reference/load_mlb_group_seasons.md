# **Load MLB group names and parents by season from the SportsDataverse data repo**

One row per MLB group per season it existed, with the name, abbreviation
and parent group **as of that season** (not today's). Published to the
`mlb_groups` release tag on the sportsdataverse-data releases. Season
keys are the single calendar year.

## Usage

``` r
load_mlb_group_seasons(..., dbConnection = NULL, tablename = NULL)
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
  try(load_mlb_group_seasons())
#> ── MLB group seasons from the SportsDataverse data repo ────────────────
#> ℹ Data updated: 2026-09-27 04:37:26 UTC
#> # A tibble: 727 × 9
#>    league group_id season level      name        short_name abbreviation
#>    <chr>  <chr>     <int> <chr>      <chr>       <chr>      <chr>       
#>  1 mlb    mlb:al     1901 conference American L… American   AL          
#>  2 mlb    mlb:al     1902 conference American L… American   AL          
#>  3 mlb    mlb:al     1903 conference American L… American   AL          
#>  4 mlb    mlb:al     1904 conference American L… American   AL          
#>  5 mlb    mlb:al     1905 conference American L… American   AL          
#>  6 mlb    mlb:al     1906 conference American L… American   AL          
#>  7 mlb    mlb:al     1907 conference American L… American   AL          
#>  8 mlb    mlb:al     1908 conference American L… American   AL          
#>  9 mlb    mlb:al     1909 conference American L… American   AL          
#> 10 mlb    mlb:al     1910 conference American L… American   AL          
#> # ℹ 717 more rows
#> # ℹ 2 more variables: parent_group_id <chr>, n_teams <int>
# }
```

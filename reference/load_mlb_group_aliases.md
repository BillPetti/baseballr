# **Load MLB group aliases from the SportsDataverse data repo**

Every name and id a source uses for an MLB group, with the seasons it is
valid for – the crosswalk from ESPN / MLB Stats API league and division
ids and names to SportsDataverse group ids. Published to the
`mlb_groups` release tag on the sportsdataverse-data releases.

## Usage

``` r
load_mlb_group_aliases(..., dbConnection = NULL, tablename = NULL)
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
| source | character | Source that uses the alias (e.g. `espn`, `mlb`, `ncaa`, `sdv`). |
| source_id | character | The source's own id for the group, when it has one. |
| name_kind | character | Alias kind: `name`, `short_name`, `abbreviation`, `slug` or `code`. |
| value | character | The alias. |
| valid_from | integer | First season the alias is valid (inclusive); `NA` = unbounded. |
| valid_to | integer | Last season the alias is valid (inclusive); `NA` = unbounded. |

## Examples

``` r
# \donttest{
  try(load_mlb_group_aliases())
#> ── MLB group aliases from the SportsDataverse data repo ────────────────
#> ℹ Data updated: 2026-09-30 14:28:13 UTC
#> # A tibble: 96 × 8
#>    league group_id  source source_id name_kind value valid_from valid_to
#>    <chr>  <chr>     <chr>  <chr>     <chr>     <chr>      <int>    <int>
#>  1 mlb    mlb:al    espn   7         abbrevia… AL          1901       NA
#>  2 mlb    mlb:al    espn   7         name      Amer…       1901       NA
#>  3 mlb    mlb:al    espn   7         short_na… AL          1901       NA
#>  4 mlb    mlb:al    mlb    103       abbrevia… AL          1901       NA
#>  5 mlb    mlb:al    mlb    103       name      Amer…       1901       NA
#>  6 mlb    mlb:al    mlb    103       short_na… Amer…       1901       NA
#>  7 mlb    mlb:al-c… espn   2         abbrevia… ALC         1994       NA
#>  8 mlb    mlb:al-c… espn   2         name      Amer…       1994       NA
#>  9 mlb    mlb:al-c… espn   2         short_na… AL C…       1994       NA
#> 10 mlb    mlb:al-c… mlb    202       abbrevia… ALC         1994       NA
#> # ℹ 86 more rows
# }
```

# **Load NCAA baseball group aliases from the SportsDataverse data repo**

Every name and id a source uses for an NCAA baseball group, with the
seasons it is valid for – the crosswalk from stats.ncaa.org conference
ids and names to SportsDataverse group ids. Published to the
`ncaa_baseball_groups` release tag on the sportsdataverse-data releases.

## Usage

``` r
load_ncaa_baseball_group_aliases(..., dbConnection = NULL, tablename = NULL)
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
  try(load_ncaa_baseball_group_aliases())
#> ── NCAA baseball group aliases from the SportsDataverse data repo ──────
#> ℹ Data updated: 2026-09-30 14:28:17 UTC
#> # A tibble: 620 × 8
#>    league  group_id source source_id name_kind value valid_from valid_to
#>    <chr>   <chr>    <chr>  <chr>     <chr>     <chr>      <int>    <int>
#>  1 ncaa_b… ncaa_ba… ncaa   821       short_na… ACC         2010     2026
#>  2 ncaa_b… ncaa_ba… ncaa   821       slug      acc           NA       NA
#>  3 ncaa_b… ncaa_ba… sdv    NA        abbrevia… ACC           NA       NA
#>  4 ncaa_b… ncaa_ba… sdv    NA        name      Atla…         NA       NA
#>  5 ncaa_b… ncaa_ba… sdv    NA        short_na… ACC           NA       NA
#>  6 ncaa_b… ncaa_ba… sdv    NA        slug      acc           NA       NA
#>  7 ncaa_b… ncaa_ba… ncaa   30022     short_na… AMCC        2010     2026
#>  8 ncaa_b… ncaa_ba… sdv    NA        abbrevia… AMCC          NA       NA
#>  9 ncaa_b… ncaa_ba… sdv    NA        name      Alle…         NA       NA
#> 10 ncaa_b… ncaa_ba… sdv    NA        short_na… AMCC          NA       NA
#> # ℹ 610 more rows
# }
```

# **Load NCAA baseball team division and conference memberships by season from the SportsDataverse data repo**

One row per NCAA baseball team per season, with the division
(`subdivision_id`) and conference it played in that season. `team_id` is
the stats.ncaa.org org id. Published to the `ncaa_baseball_groups`
release tag on the sportsdataverse-data releases, one file per season.

## Usage

``` r
load_ncaa_baseball_team_group_seasons(
  seasons = most_recent_ncaa_baseball_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit seasons (the spring year), or `TRUE` for every
  published season. (Min: 2010)

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
| season | integer | Season. |
| team_id | character | ESPN team id where ESPN covers the team, otherwise the league's own id. |
| team_id_source | character | Id system of `team_id` (e.g. `espn`, `mlb`, `ncaa_org`). |
| team_name | character | Team name as of that season. |
| subdivision_id | character | SportsDataverse subdivision group id; `NA` where the level does not apply. |
| conference_id | character | SportsDataverse conference group id (for MLB, the American or National League). |
| division_id | character | SportsDataverse division group id; `NA` where the level does not apply. |
| source | character | Source the membership came from. |
| sources_agree | logical | Whether a second source agrees; `NA` when only one source covers the season. |
| notes | character | Notes, e.g. the league's own team id. |

## Examples

``` r
# \donttest{
  try(load_ncaa_baseball_team_group_seasons(seasons = 2025))
#> ── NCAA baseball team group seasons from the SportsDataverse data repo ─
#> ℹ Data updated: 2026-10-09 04:42:16 UTC
#> # A tibble: 943 × 11
#>    league        season team_id team_id_source team_name  subdivision_id
#>    <chr>          <int> <chr>   <chr>          <chr>      <chr>         
#>  1 ncaa_baseball   2025 10      ncaa_org       UAH        ncaa_baseball…
#>  2 ncaa_baseball   2025 100     ncaa_org       Cal State… ncaa_baseball…
#>  3 ncaa_baseball   2025 1000    ncaa_org       Carson-Ne… ncaa_baseball…
#>  4 ncaa_baseball   2025 1001    ncaa_org       Catawba    ncaa_baseball…
#>  5 ncaa_baseball   2025 1004    ncaa_org       Central A… ncaa_baseball…
#>  6 ncaa_baseball   2025 1009    ncaa_org       Central O… ncaa_baseball…
#>  7 ncaa_baseball   2025 101     ncaa_org       CSUN       ncaa_baseball…
#>  8 ncaa_baseball   2025 1010    ncaa_org       Central W… ncaa_baseball…
#>  9 ncaa_baseball   2025 1013    ncaa_org       Charlesto… ncaa_baseball…
#> 10 ncaa_baseball   2025 1014    ncaa_org       Col. of C… ncaa_baseball…
#> # ℹ 933 more rows
#> # ℹ 5 more variables: conference_id <chr>, division_id <chr>,
#> #   source <chr>, sources_agree <lgl>, notes <chr>
# }
```

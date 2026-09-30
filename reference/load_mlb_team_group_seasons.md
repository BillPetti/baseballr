# **Load MLB team league and division memberships by season from the SportsDataverse data repo**

One row per MLB team per season, with the league (`conference_id`) and
division it played in that season, e.g. the Houston Astros in
`mlb:nl-central` for 2012 and `mlb:al-west` from 2013. Published to the
`mlb_groups` release tag on the sportsdataverse-data releases, one file
per season.

## Usage

``` r
load_mlb_team_group_seasons(
  seasons = most_recent_mlb_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit seasons, or `TRUE` for every published season.
  (Min: 1901)

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
  try(load_mlb_team_group_seasons(seasons = 2013))
#> ── MLB team group seasons from the SportsDataverse data repo ───────────
#> ℹ Data updated: 2026-09-30 14:28:16 UTC
#> # A tibble: 30 × 11
#>    league season team_id team_id_source team_name         subdivision_id
#>    <chr>   <int> <chr>   <chr>          <chr>             <chr>         
#>  1 mlb      2013 1       espn           Baltimore Orioles NA            
#>  2 mlb      2013 10      espn           New York Yankees  NA            
#>  3 mlb      2013 11      espn           Oakland Athletics NA            
#>  4 mlb      2013 12      espn           Seattle Mariners  NA            
#>  5 mlb      2013 13      espn           Texas Rangers     NA            
#>  6 mlb      2013 14      espn           Toronto Blue Jays NA            
#>  7 mlb      2013 15      espn           Atlanta Braves    NA            
#>  8 mlb      2013 16      espn           Chicago Cubs      NA            
#>  9 mlb      2013 17      espn           Cincinnati Reds   NA            
#> 10 mlb      2013 18      espn           Houston Astros    NA            
#> # ℹ 20 more rows
#> # ℹ 5 more variables: conference_id <chr>, division_id <chr>,
#> #   source <chr>, sources_agree <lgl>, notes <chr>
# }
```

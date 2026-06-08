# **Load cleaned NCAA baseball schedule from the baseballr data repo**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_ncaa_baseball_schedule(
  seasons = most_recent_ncaa_baseball_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given NCAA college baseball
  seasons. (Min: 2012)

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by

- tablename:

  The name of the schedule data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_ncaa_baseball_schedule(seasons = 2022))
#> ── NCAA Schedule Information from baseballr data repository ────────────
#> ℹ Data updated: 2023-03-05 06:06:15 UTC
#> # A tibble: 23,153 × 24
#>     year season_id date       home_team     home_team_id home_team_score
#>    <int>     <int> <chr>      <chr>                <int>           <int>
#>  1  2022     15860 02/01/2022 Carson-Newman         1000               5
#>  2  2022     15860 02/01/2022 Barton               15646               5
#>  3  2022     15860 02/01/2022 Stanislaus S…          103               5
#>  4  2022     15860 02/01/2022 Savannah St.           632               4
#>  5  2022     15860 02/01/2022 Belmont Abbey         2683              10
#>  6  2022     15860 02/01/2022 Holy Names           30175               2
#>  7  2022     15860 02/01/2022 Southwest (N…           NA               1
#>  8  2022     15860 02/01/2022 AUM                  30093               7
#>  9  2022     15860 02/01/2022 Shorter              30151               3
#> 10  2022     15860 02/01/2022 West Ala.              358               1
#> # ℹ 23,143 more rows
#> # ℹ 18 more variables: home_team_conference <chr>,
#> #   home_team_conference_id <int>, home_team_slug <chr>,
#> #   home_team_division <int>, away_team <chr>, away_team_id <int>,
#> #   away_team_score <int>, away_team_conference <chr>,
#> #   away_team_conference_id <int>, away_team_slug <chr>,
#> #   away_team_division <int>, neutral_site <chr>, innings <int>, …
# }
```

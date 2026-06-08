# **Get NCAA Baseball Rosters**

**Get NCAA Baseball Rosters**

## Usage

``` r
ncaa_roster(team_id = NULL, year, ...)
```

## Arguments

- team_id:

  NCAA id for a school

- year:

  The year of interest

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame containing roster information, including IDs and urls for
each player (if available)

|               |           |
|---------------|-----------|
| col_name      | types     |
| player_name   | character |
| class         | character |
| player_id     | character |
| season        | numeric   |
| number        | character |
| position      | character |
| player_url    | character |
| team_name     | character |
| conference    | character |
| team_id       | numeric   |
| division      | numeric   |
| conference_id | numeric   |

## Examples

``` r
# \donttest{
  try(ncaa_roster(team_id = 104, year = 2023))
#> ── NCAA Baseball Roster data from stats.ncaa.org ──── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:21:34 UTC
#> # A tibble: 43 × 16
#>    player_name class player_id season number position player_url team_id
#>    <chr>       <chr> <chr>      <dbl> <chr>  <chr>    <chr>        <dbl>
#>  1 Ager, Matt  So    /players…   2023 34     P        https://s…     104
#>  2 Barnier, L… So    NA          2023 24     P        NA             104
#>  3 Barrett, H… Fr    /players…   2023 36     UT       https://s…     104
#>  4 Benbrook, … Jr    /players…   2023 51     P        https://s…     104
#>  5 Bolt, Jack  Fr    NA          2023 29     P        NA             104
#>  6 Bremner, T… Fr    /players…   2023 37     P        https://s…     104
#>  7 Brethowr, … So    /players…   2023 52     OF       https://s…     104
#>  8 Brown, Jes… So    /players…   2023 3      UT       https://s…     104
#>  9 Callahan, … Jr    /players…   2023 23     P        https://s…     104
#> 10 Camarillo,… Fr    /players…   2023 22     P        https://s…     104
#> # ℹ 33 more rows
#> # ℹ 8 more variables: team_name <chr>, team_url <chr>,
#> #   conference_id <dbl>, conference <chr>, division <dbl>, year <dbl>,
#> #   season_id <dbl>, season_team_id <dbl>
# }
```

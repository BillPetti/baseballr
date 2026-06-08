# **Lookup NCAA baseball school IDs (Division I, II, and III)**

This function allows the user to look up the `team_id` needed for the
[`ncaa_team_player_stats()`](https://billpetti.github.io/baseballr/reference/ncaa_team_player_stats.md)
function.

## Usage

``` r
ncaa_school_id_lu(team_name = NULL)
```

## Arguments

- team_name:

  A string that will be searched for in the names of the teams.

## Value

Returns a tibble with school identification data: team_id, team_name,
team_url, conference, conference_id, division, year, and season_id

|               |           |
|---------------|-----------|
| col_name      | types     |
| team_id       | numeric   |
| team_name     | character |
| team_url      | character |
| conference_id | numeric   |
| conference    | character |
| division      | numeric   |
| year          | numeric   |
| season_id     | numeric   |

## Examples

``` r
# \donttest{
  try(ncaa_school_id_lu("Van"))
#> ── NCAA Baseball Teams Information from baseballr data repository ──────
#> ℹ Data updated: 2026-05-30 01:50:36 UTC
#> # A tibble: 20 × 9
#>    team_id team_name  team_url   conference_id conference division  year
#>      <dbl> <chr>      <chr>              <dbl> <chr>         <dbl> <dbl>
#>  1     736 Vanderbilt /teams/61…           911 SEC               1  2026
#>  2     736 Vanderbilt /teams/59…           911 SEC               1  2025
#>  3     736 Vanderbilt /team/736…           911 SEC               1  2024
#>  4     736 Vanderbilt /team/736…           911 SEC               1  2023
#>  5     736 Vanderbilt /team/736…           911 SEC               1  2022
#>  6     736 Vanderbilt /team/736…           911 SEC               1  2021
#>  7     736 Vanderbilt /team/736…           911 SEC               1  2020
#>  8     736 Vanderbilt /team/736…           911 SEC               1  2019
#>  9     736 Vanderbilt /team/736…           911 SEC               1  2018
#> 10     736 Vanderbilt /team/736…           911 SEC               1  2017
#> 11     736 Vanderbilt /team/736…           911 SEC               1  2016
#> 12     736 Vanderbilt /team/736…           911 SEC               1  2015
#> 13     736 Vanderbilt /team/736…           911 SEC               1  2014
#> 14     736 Vanderbilt /team/736…           911 SEC               1  2013
#> 15     736 Vanderbilt /team/736…           911 SEC               1  2012
#> 16     736 Vanderbilt /team/736…           911 SEC               1  2011
#> 17     736 Vanderbilt /team/736…           911 SEC               1  2010
#> 18   30263 Vanguard   /teams/61…         19591 PacWest           2  2026
#> 19   30263 Vanguard   /teams/59…         19591 PacWest           2  2025
#> 20   30263 Vanguard   /team/302…         99010 DII Indep…        2  2024
#> # ℹ 2 more variables: season_id <dbl>, season_team_id <dbl>
# }
```

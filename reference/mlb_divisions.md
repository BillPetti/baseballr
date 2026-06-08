# **MLB Divisions**

**MLB Divisions**

## Usage

``` r
mlb_divisions(division_id = NULL, league_id = NULL, sport_id = NULL)
```

## Arguments

- division_id:

  Return division(s) data for a specific division

- league_id:

  Return division(s) data for all divisions in a specific league

- sport_id:

  Return division(s) for all divisions in a specific sport.

## Value

Returns a tibble with the following columns

|                       |           |                                            |
|-----------------------|-----------|--------------------------------------------|
| col_name              | types     | description                                |
| division_id           | integer   | MLB division ID.                           |
| division_name         | character | Division name.                             |
| season                | character | Season (YYYY).                             |
| division_name_short   | character | Short division name.                       |
| division_link         | character | MLB Stats API relative division link.      |
| division_abbreviation | character | Division abbreviation.                     |
| has_wildcard          | logical   | Whether the division has a wild card.      |
| sort_order            | integer   | Display sort order for the division.       |
| num_playoff_teams     | integer   | Number of playoff teams from the division. |
| active                | logical   | Whether the division is currently active.  |
| league_id             | integer   | MLB league ID.                             |
| league_link           | character | MLB Stats API relative league link.        |
| sport_id              | integer   | MLB sport ID.                              |
| sport_link            | character | MLB Stats API relative sport link.         |

## Examples

``` r
# \donttest{
  try(mlb_divisions(sport_id = 1))
#> ── MLB Divisions data from MLB.com ────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:45:41 UTC
#> # A tibble: 6 × 14
#>   division_id division_name     season division_name_short division_link
#>         <int> <chr>             <chr>  <chr>               <chr>        
#> 1         200 American League … 2026   AL West             /api/v1/divi…
#> 2         201 American League … 2026   AL East             /api/v1/divi…
#> 3         202 American League … 2026   AL Central          /api/v1/divi…
#> 4         203 National League … 2026   NL West             /api/v1/divi…
#> 5         204 National League … 2026   NL East             /api/v1/divi…
#> 6         205 National League … 2026   NL Central          /api/v1/divi…
#> # ℹ 9 more variables: division_abbreviation <chr>, has_wildcard <lgl>,
#> #   sort_order <int>, num_playoff_teams <int>, active <lgl>,
#> #   league_id <int>, league_link <chr>, sport_id <int>,
#> #   sport_link <chr>
# }
```

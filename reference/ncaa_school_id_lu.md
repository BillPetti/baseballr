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

|               |           |                                   |
|---------------|-----------|-----------------------------------|
| col_name      | types     | description                       |
| team_id       | numeric   | Team NCAA id.                     |
| team_name     | character | Team name.                        |
| team_url      | character | Relative stats.ncaa.org team url. |
| conference_id | numeric   | Conference identifier.            |
| conference    | character | Conference name.                  |
| division      | numeric   | NCAA division (1, 2, 3).          |
| year          | numeric   | Season (4-digit year).            |
| season_id     | numeric   | stats.ncaa.org season identifier. |

## Details

Example usage (resolved from the bundled NCAA teams table):

    ncaa_school_id_lu("Van")

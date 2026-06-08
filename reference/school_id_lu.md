# **(legacy) Lookup NCAA baseball school IDs (Division I, II, and III)**

**(legacy) Lookup NCAA baseball school IDs (Division I, II, and III)**

## Usage

``` r
school_id_lu(team_name = NULL)
```

## Arguments

- team_name:

  A string that will be searched for in the names of the teams.

## Value

Returns a tibble with school identification data: team_id, team_name,
team_url, conference, conference_id, division, year, and season_id

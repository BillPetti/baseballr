# **Calculate Team-level Consistency**

This function allows you to calculate team-level consistency in run
scoring and run prevention over the course of an entire season.

## Usage

``` r
team_consistency(year)
```

## Arguments

- year:

  Season consistency should be run for.

## Value

Returns a tibble with the following columns

|              |           |
|--------------|-----------|
| col_name     | types     |
| Team         | character |
| Con_R        | numeric   |
| Con_RA       | numeric   |
| Con_R_Ptile  | numeric   |
| Con_RA_Ptile | numeric   |

## Details

      try(team_consistency(year=2021))

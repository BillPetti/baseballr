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

|              |           |                                                  |
|--------------|-----------|--------------------------------------------------|
| col_name     | types     | description                                      |
| Team         | character | Team name.                                       |
| Con_R        | numeric   | Run-scoring consistency score for the season.    |
| Con_RA       | numeric   | Run-prevention (runs allowed) consistency score. |
| Con_R_Ptile  | numeric   | Percentile rank of run-scoring consistency.      |
| Con_RA_Ptile | numeric   | Percentile rank of run-prevention consistency.   |

## Details

      try(team_consistency(year=2021))

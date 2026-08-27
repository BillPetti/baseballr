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
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_int_vector' from package 'arrow'; returning length zero vector
#> Warning: Column 1 ['year'] of item 1 is length 0. This (and 23 others like it) has been filled with NA (NULL for list columns) to make each item uniform.
#> ───────────────────────────────────────────────────── baseballr 2.0.0 ──
#> # A tibble: 23,153 × 25
#>     year season_id date  home_team home_team_id home_team_score
#>    <int>     <int> <chr> <chr>            <int>           <int>
#>  1    NA        NA NA    NA                  NA              NA
#>  2    NA        NA NA    NA                  NA              NA
#>  3    NA        NA NA    NA                  NA              NA
#>  4    NA        NA NA    NA                  NA              NA
#>  5    NA        NA NA    NA                  NA              NA
#>  6    NA        NA NA    NA                  NA              NA
#>  7    NA        NA NA    NA                  NA              NA
#>  8    NA        NA NA    NA                  NA              NA
#>  9    NA        NA NA    NA                  NA              NA
#> 10    NA        NA NA    NA                  NA              NA
#> # ℹ 23,143 more rows
#> # ℹ 19 more variables: home_team_conference <chr>,
#> #   home_team_conference_id <int>, home_team_slug <chr>,
#> #   home_team_division <int>, away_team <chr>, away_team_id <int>,
#> #   away_team_score <int>, away_team_conference <chr>,
#> #   away_team_conference_id <int>, away_team_slug <chr>,
#> #   away_team_division <int>, neutral_site <chr>, innings <int>, …
# }
```

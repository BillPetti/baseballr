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
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: cannot unserialize ALTVEC object of class 'arrow::array_large_string_vector' from package 'arrow'; returning length zero vector
#> Warning: Column 1 ['team_id'] of item 1 is length 0. This (and 7 others like it) has been filled with NA (NULL for list columns) to make each item uniform.
#> ───────────────────────────────────────────────────── baseballr 2.1.0 ──
#> # A tibble: 45,038 × 14
#>    team_id team_name date  game_number opponent_id opponent result
#>    <chr>   <chr>     <chr>       <int> <chr>       <chr>    <chr> 
#>  1 NA      NA        NA             NA NA          NA       NA    
#>  2 NA      NA        NA             NA NA          NA       NA    
#>  3 NA      NA        NA             NA NA          NA       NA    
#>  4 NA      NA        NA             NA NA          NA       NA    
#>  5 NA      NA        NA              1 NA          NA       NA    
#>  6 NA      NA        NA              2 NA          NA       NA    
#>  7 NA      NA        NA             NA NA          NA       NA    
#>  8 NA      NA        NA             NA NA          NA       NA    
#>  9 NA      NA        NA             NA NA          NA       NA    
#> 10 NA      NA        NA             NA NA          NA       NA    
#> # ℹ 45,028 more rows
#> # ℹ 7 more variables: outcome <chr>, team_score <int>,
#> #   opponent_score <int>, contest_id <chr>, attendance <int>,
#> #   division <int>, season <int>
# }
```

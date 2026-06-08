# **Generate linear weight values for events using Baseball Savant data**

This function allows a user to generate linear weight values for events
using Baseball Savant data. Output includes both linear weights above
average and linear weights above outs for home runs, triples, doubles,
singles, walks, hit by pitches, and outs.

## Usage

``` r
linear_weights_savant(df, level = "plate appearance")
```

## Arguments

- df:

  A data frame generated from Baseball Savant that has been run through
  the
  [`run_expectancy_code()`](https://billpetti.github.io/baseballr/reference/run_expectancy_code.md)
  function.

- level:

  Whether to calculate linear weights the plate appearance or pitch
  level. Defaults to 'plate appearance'.

## Value

Returns a tibble with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| events | character | Event type (e.g. home_run, triple, double, single, walk, hit_by_pitch, outs). |
| linear_weights_above_average | numeric | Average change in run expectancy (RE24) for the event, i.e. linear weight above average. |
| linear_weights_above_outs | numeric | Linear weight expressed above the value of an out (above-average value plus the absolute out value). |

## Examples

``` r
# \donttest{
 try({
   df <- statcast_search(start_date = "2016-04-06", end_date = "2016-04-15", 
                         playerid = 621043, player_type = 'batter') 
   df <- run_expectancy_code(df, level = "plate appearances")
   linear_weights_savant(df, level = "plate appearance")
 })
#> # A tibble: 5 × 3
#>   events   linear_weights_above_average linear_weights_above_outs
#>   <chr>                           <dbl>                     <dbl>
#> 1 home_run                         1                         1   
#> 2 double                           0.5                       0.5 
#> 3 single                           0.17                      0.17
#> 4 walk                             0                         0   
#> 5 outs                             0                         0   
# }
```

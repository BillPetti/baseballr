# **Label Statcast data as imputed**

Based on a series of heuristics, this function attempts to label
Statcast data for which the launch angle and speed have been imputed.

## Usage

``` r
label_statcast_imputed_data(
  statcast_df,
  impute_file = NULL,
  inverse_precision = 10000
)
```

## Arguments

- statcast_df:

  A dataframe containing Statcast batted ball data

- impute_file:

  A CSV file giving the launch angle, launch speed, `bb_type`, events
  fields to label as imputed. if NULL then it's read from the `extdata`
  folder of the package.

- inverse_precision:

  inverse of how many digits to truncate the launch angle and speed to
  for comparison. Default is `10000`, i.e. keep 4 digits of precision.

## Value

A copy of the input dataframe with the same Statcast pitch-level columns
produced by
[`statcast_search()`](https://billpetti.github.io/baseballr/reference/statcast_search.md)
(see that function's return value for the full column-by-column
reference), with three columns appended:

|  |  |  |
|----|----|----|
| col_name | types | description |
| ila | integer | Launch angle truncated to integer precision (`launch_angle * inverse_precision`), used as a join key against the impute table. |
| ils | integer | Launch speed truncated to integer precision (`launch_speed * inverse_precision`), used as a join key against the impute table. |
| imputed | numeric | 1 if the launch angle and launch speed are likely imputed, 0 otherwise. |

## Examples

``` r
# \donttest{
  try({
    statcast_df <- statcast_search("2017-05-01", "2017-05-02")
    sc_df <- label_statcast_imputed_data(statcast_df)
    mean(sc_df$imputed)
  })
#> [1] 0.003550596
# }
```

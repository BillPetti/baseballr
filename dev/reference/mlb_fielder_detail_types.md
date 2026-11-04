# **MLB Fielder Detail Types**

**MLB Fielder Detail Types**

## Usage

``` r
mlb_fielder_detail_types()
```

## Value

Returns a tibble with the following columns

|           |           |                                                 |
|-----------|-----------|-------------------------------------------------|
| col_name  | types     | description                                     |
| stat_name | character | Internal fielder detail stat name.              |
| code      | character | Fielder detail type code.                       |
| names     | list      | Associated detail names for the type.           |
| chance    | logical   | Whether the detail counts as a fielding chance. |
| error     | logical   | Whether the detail counts as an error.          |

## Examples

``` r
# \donttest{
  try(mlb_fielder_detail_types())
#> Warning: corrupt data frame: columns will be truncated or padded with NAs
#>                            stat_name code
#> 1                           f_assist    A
#> 2                        f_assist_of  FOF
#> 3                     f_fielded_ball <NA>
#> 4                       f_deflection <NA>
#> 5                           f_putout   PO
#> 6                            f_touch <NA>
#> 7                            f_error    E
#> 8                   f_fielding_error <NA>
#> 9                       f_foul_error <NA>
#> 10              f_error_dropped_ball <NA>
#> 11                    f_interference <NA>
#> 12                  f_throwing_error <NA>
#> 13                  c_catcher_interf <NA>
#> 14                     t_double_play <NA>
#> 15                     t_triple_play <NA>
#> 16 f_defensive_shift_violation_error <NA>
#>                                                      names chance error
#> 1                 \033[38;5;246m# A tibble: 16 × 1\033[39m   TRUE FALSE
#> 2                                           names           FALSE FALSE
#> 3      \033[3m\033[38;5;246m<chr>\033[39m\033[23m           FALSE FALSE
#> 4                  \033[38;5;250m 1\033[39m assist          FALSE FALSE
#> 5                  \033[38;5;250m 2\033[39m outfieldAssist   TRUE FALSE
#> 6  \033[38;5;250m 3\033[39m \033[31mNA\033[39m              FALSE FALSE
#> 7  \033[38;5;250m 4\033[39m \033[31mNA\033[39m               TRUE  TRUE
#> 8                  \033[38;5;250m 5\033[39m putout           TRUE  TRUE
#> 9  \033[38;5;250m 6\033[39m \033[31mNA\033[39m               TRUE  TRUE
#> 10                 \033[38;5;250m 7\033[39m error            TRUE  TRUE
#> 11 \033[38;5;250m 8\033[39m \033[31mNA\033[39m               TRUE  TRUE
#> 12 \033[38;5;250m 9\033[39m \033[31mNA\033[39m               TRUE  TRUE
#> 13 \033[38;5;250m10\033[39m \033[31mNA\033[39m              FALSE  TRUE
#> 14 \033[38;5;250m11\033[39m \033[31mNA\033[39m              FALSE FALSE
#> 15 \033[38;5;250m12\033[39m \033[31mNA\033[39m              FALSE FALSE
#> 16 \033[38;5;250m13\033[39m \033[31mNA\033[39m              FALSE  TRUE
# }
```

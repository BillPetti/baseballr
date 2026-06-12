# **MLB Runner Detail Types**

**MLB Runner Detail Types**

## Usage

``` r
mlb_runner_detail_types()
```

## Value

Returns a tibble with the following columns

|           |           |                                                      |
|-----------|-----------|------------------------------------------------------|
| col_name  | types     | description                                          |
| stat_name | character | Name of the runner detail stat (e.g. 'r_force_out'). |

## Examples

``` r
# \donttest{
  try(mlb_runner_detail_types())
#> ── MLB Runner Detail Types data from MLB.com ──────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:42:48 UTC
#> # A tibble: 31 × 1
#>    stat_name          
#>    <chr>              
#>  1 r_force_out        
#>  2 r_adv_force        
#>  3 r_adv_throw        
#>  4 r_doubled_off      
#>  5 r_thrown_out       
#>  6 r_tagged_out       
#>  7 r_out_stretching   
#>  8 r_appeal_left_early
#>  9 r_rfc              
#> 10 r_out_appeal       
#> # ℹ 21 more rows
# }
```

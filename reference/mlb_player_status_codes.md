# **MLB Player Status Codes**

**MLB Player Status Codes**

## Usage

``` r
mlb_player_status_codes()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_status_code | character | Short code for the player status (e.g. 'A'). |
| player_status_description | character | Description of the player status (e.g. 'Active'). |

## Examples

``` r
# \donttest{
  try(mlb_player_status_codes())
#> ── MLB Player Status Codes data from MLB.com ──────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:42:46 UTC
#> # A tibble: 36 × 2
#>    player_status_code player_status_description
#>    <chr>              <chr>                    
#>  1 A                  Active                   
#>  2 D7                 7-day IL                 
#>  3 D10                10-day IL                
#>  4 D15                15-day IL                
#>  5 D60                60-day IL                
#>  6 D0                 IL                       
#>  7 ILF                Full Season IL           
#>  8 RM                 Reassigned               
#>  9 RA                 Rehab                    
#> 10 DEV                Development List         
#> # ℹ 26 more rows
# }
```

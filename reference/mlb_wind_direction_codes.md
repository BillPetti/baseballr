# **MLB Wind Direction Codes**

**MLB Wind Direction Codes**

## Usage

``` r
mlb_wind_direction_codes()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| wind_direction_code | character | Wind direction code (e.g., In From CF). |
| wind_direction_description | character | Wind direction description. |

## Examples

``` r
# \donttest{
  try(mlb_wind_direction_codes())
#> ── MLB Wind Direction Codes data from MLB.com ─────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 14:09:59 UTC
#> # A tibble: 11 × 2
#>    wind_direction_code wind_direction_description
#>    <chr>               <chr>                     
#>  1 Calm                Calm                      
#>  2 In From CF          In From CF                
#>  3 In From LF          In From LF                
#>  4 In From RF          In From RF                
#>  5 L To R              L To R                    
#>  6 None                None                      
#>  7 Out To CF           Out To CF                 
#>  8 Out To LF           Out To LF                 
#>  9 Out To RF           Out To RF                 
#> 10 R To L              R To L                    
#> 11 Varies              Varies                    
# }
```

# **MLB Sky (Weather) Codes**

**MLB Sky (Weather) Codes**

## Usage

``` r
mlb_sky()
```

## Value

Returns a tibble with the following columns

|                 |           |                                                  |
|-----------------|-----------|--------------------------------------------------|
| col_name        | types     | description                                      |
| sky_code        | character | Code for the sky/weather condition.              |
| sky_description | character | Description of the sky condition (e.g. 'Clear'). |

## Examples

``` r
# \donttest{
  try(mlb_sky())
#> ── MLB Sky (Weather) Codes data from MLB.com ──────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:25:08 UTC
#> # A tibble: 10 × 2
#>    sky_code      sky_description
#>    <chr>         <chr>          
#>  1 Clear         Clear          
#>  2 Cloudy        Cloudy         
#>  3 Dome          Dome           
#>  4 Drizzle       Drizzle        
#>  5 Overcast      Overcast       
#>  6 Partly Cloudy Partly Cloudy  
#>  7 Rain          Rain           
#>  8 Roof Closed   Roof Closed    
#>  9 Snow          Snow           
#> 10 Sunny         Sunny          
# }
```

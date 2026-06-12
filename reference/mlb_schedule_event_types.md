# **MLB Schedule Event Types**

**MLB Schedule Event Types**

## Usage

``` r
mlb_schedule_event_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| schedule_event_type_code | character | Short code for the schedule event type. |
| schedule_event_type_name | character | Name of the event type (e.g. 'All-Star Weekend Event'). |

## Examples

``` r
# \donttest{
  try(mlb_schedule_event_types())
#> ── MLB Schedule Event Types data from MLB.com ─────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:42:51 UTC
#> # A tibble: 19 × 2
#>    schedule_event_type_code schedule_event_type_name          
#>    <chr>                    <chr>                             
#>  1 A                        All-Star Weekend Event            
#>  2 T                        Team Event                        
#>  3 E                        Exhibition                        
#>  4 Z                        Postseason Games                  
#>  5 Y                        Spring Training Games             
#>  6 W                        Pitchers & Catchers Report        
#>  7 X                        Full Squad Reports                
#>  8 H                        STH Events                        
#>  9 B                        Ballpark Tours                    
#> 10 I                        Important Dates                   
#> 11 O                        Other                             
#> 12 C                        Cultural Events                   
#> 13 D                        Tracking Data Events              
#> 14 F                        Festival                          
#> 15 K                        Kids & Family                     
#> 16 M                        Music                             
#> 17 P                        Promotion Logo - Background Image 
#> 18 Q                        Promotion Logo - Single Date Image
#> 19 S                        Studio Event                      
# }
```

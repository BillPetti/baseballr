# **MLB Hit Trajectories**

**MLB Hit Trajectories**

## Usage

``` r
mlb_hit_trajectories()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| hit_trajectory_code | character | Hit trajectory code (e.g. 'bunt_grounder', 'bunt_popup'). |
| hit_trajectory_description | character | Hit trajectory description (e.g. 'Bunt - Ground Ball', 'Bunt - Popup'). |

## Examples

``` r
# \donttest{
  try(mlb_hit_trajectories())
#> ── MLB Hit Trajectories data from MLB.com ─────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-24 02:06:12 UTC
#> # A tibble: 7 × 2
#>   hit_trajectory_code hit_trajectory_description
#>   <chr>               <chr>                     
#> 1 bunt_grounder       Bunt - Ground Ball        
#> 2 bunt_popup          Bunt - Popup              
#> 3 bunt_line_drive     Bunt - Line Drive         
#> 4 line_drive          Line Drive                
#> 5 ground_ball         Ground Ball               
#> 6 fly_ball            Fly Ball                  
#> 7 popup               Popup                     
# }
```

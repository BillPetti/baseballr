# **MLB Roster Types**

**MLB Roster Types**

## Usage

``` r
mlb_roster_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| roster_type_description | character | Description of the roster type. |
| roster_type_lookup_name | character | Lookup name for the roster type. |
| roster_type_parameter | character | Parameter value to pass as `roster_type` in API calls. |

## Examples

``` r
# \donttest{
  try(mlb_roster_types())
#> ── MLB Roster Types data from MLB.com ─────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:24:07 UTC
#> # A tibble: 9 × 3
#>   roster_type_description   roster_type_lookup_n…¹ roster_type_parameter
#>   <chr>                     <chr>                  <chr>                
#> 1 40 man roster for a team  40man                  40Man                
#> 2 Full roster including ac… fullSeason             fullSeason           
#> 3 Full roster including ac… full                   fullRoster           
#> 4 Non-Roster Invitees       nonRosterInvitees      nonRosterInvitees    
#> 5 Active roster for a team  active                 active               
#> 6 All Time roster for a te… alltime                allTime              
#> 7 Depth chart for a team    active                 depthChart           
#> 8 Roster for day of game    active                 gameday              
#> 9 Coach roster for a team   active                 coach                
#> # ℹ abbreviated name: ¹​roster_type_lookup_name
# }
```

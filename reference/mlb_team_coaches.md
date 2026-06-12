# **MLB Team Coaches**

**MLB Team Coaches**

## Usage

``` r
mlb_team_coaches(team_id = NULL, date = NULL, season = NULL)
```

## Arguments

- team_id:

  Team ID to return team coach information for.

- date:

  Date to return team coach information for.

- season:

  Year to return team coach information for.

## Value

Returns a tibble with the following columns

|                  |           |                           |
|------------------|-----------|---------------------------|
| col_name         | types     | description               |
| jersey_number    | character | Coach uniform number.     |
| job              | character | Job name (e.g., Manager). |
| job_id           | character | Job code identifier.      |
| title            | character | Full coaching title.      |
| person_id        | integer   | Coach MLBAM person ID.    |
| person_full_name | character | Coach full name.          |
| person_link      | character | API link to the coach.    |

## Examples

``` r
# \donttest{
  try(mlb_team_coaches(team_id = 137, season = 2021))
#> ── MLB Team Coaches data from MLB.com ─────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:16:15 UTC
#> # A tibble: 17 × 7
#>    jersey_number job             job_id title person_id person_full_name
#>    <chr>         <chr>           <chr>  <chr>     <int> <chr>           
#>  1 19            Manager         MNGR   Mana…    137002 Gabe Kapler     
#>  2 0             Bullpen Coach   COAU   Bull…    503360 Craig Albernaz  
#>  3 84            Pitching Coach  COAP   Pitc…    457732 Andrew Bailey   
#>  4 97            Pitching Coach  COAP   Dire…    446454 Brian Bannister 
#>  5 50            Bench Coach     COAB   Benc…    678989 Kai Correa      
#>  6 82            Hitting Coach   COAT   Hitt…    452778 Donnie Ecker    
#>  7 88            Assistant Hitt… COAA   Dire…    682508 Dustin Lind     
#>  8 87            Assistant Pitc… COPA   Assi…    448258 J.P. Martinez   
#>  9 92            Major League C… MAJC   Majo…    693136 Alyssa Nakken   
#> 10 98            Coach           COAC   Dire…    445010 Fernando Perez  
#> 11 21            Quality Contro… QUAC   Qual…    455512 Nick Ortiz      
#> 12 0             First Base Coa… COA1   Firs…    460322 Antoan Richards…
#> 13 77            Hitting Coach   COAT   Hitt…    643585 Justin Viele    
#> 14 96            Assistant Coach ASSC   Assi…    657804 Brant Whiting   
#> 15 23            Third Base Coa… COA3   Thir…    124588 Ron Wotus       
#> 16 91            Major League C… MAJC   Majo…    506912 Mark Hallberg   
#> 17 99            Bullpen Catcher BCAT   Bull…    666065 Taira Uematsu   
#> # ℹ 1 more variable: person_link <chr>
# }
```

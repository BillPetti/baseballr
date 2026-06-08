# **MLB Team Personnel**

**MLB Team Personnel**

## Usage

``` r
mlb_team_personnel(team_id = NULL, date = NULL)
```

## Arguments

- team_id:

  Team ID to return team coach information for.

- date:

  Date to return team coach information for.

## Value

Returns a tibble with the following columns

|                  |           |                                     |
|------------------|-----------|-------------------------------------|
| col_name         | types     | description                         |
| jersey_number    | character | Personnel uniform number.           |
| job              | character | Job name (e.g., Special Assistant). |
| job_id           | character | Job code identifier.                |
| title            | character | Full personnel title.               |
| person_id        | integer   | Personnel MLBAM person ID.          |
| person_full_name | character | Personnel full name.                |
| person_link      | character | API link to the person.             |

## Examples

``` r
# \donttest{
  try(mlb_team_personnel(team_id = 137, date = "08/28/2016"))
#> ── MLB Team Personnel data from MLB.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 04:40:34 UTC
#> # A tibble: 15 × 7
#>    jersey_number job             job_id title person_id person_full_name
#>    <chr>         <chr>           <chr>  <chr>     <int> <chr>           
#>  1 "21"          Special Assist… SASU   Spec…    113645 Shawon Dunston  
#>  2 "91"          Special Assist… SASU   Spec…    666066 Chad Chop       
#>  3 ""            General Manager GMGR   Gene…    699624 Bobby Evans     
#>  4 ""            Head Athletic … HATR   Seni…    599861 Dave Groeschner 
#>  5 ""            Assistant Trai… ATRA   Head…    579868 Anthony Reyes   
#>  6 ""            Strength and C… SCCC   Stre…    580105 Carl Kochan     
#>  7 ""            Physical Thera… PTHR   Phys…    580103 Tony Reale      
#>  8 ""            Director of Te… DTET   Seni…    640157 Bret Alexander  
#>  9 ""            Equipment Mana… EQUP   Equi…    599864 Mike Murphy     
#> 10 ""            Coordinator, O… CORT   Coor…    599866 Michael Scardino
#> 11 ""            Coordinator, B… CBAS   Coor…    112724 Henry Cotto     
#> 12 ""            Coordinator, C… CCAT   Coor…    118267 Kirt Manwaring  
#> 13 ""            Coordinator, P… CPIT   Coor…    111351 Bert Bradley    
#> 14 ""            Strength and C… SCCR   Stre…    580104 Geoff Head      
#> 15 ""            Assistant Trai… ATRA   Assi…    599862 Mark Gruesbeck  
#> # ℹ 1 more variable: person_link <chr>
# }
```

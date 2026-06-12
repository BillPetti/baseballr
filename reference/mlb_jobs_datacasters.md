# **MLB Jobs Datacasters**

**MLB Jobs Datacasters**

## Usage

``` r
mlb_jobs_datacasters(sport_id = NULL, date = NULL)
```

## Arguments

- sport_id:

  Return information for a given sport_id.

- date:

  Return information for a given date.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| jersey_number | character | Jersey number (typically blank for datacasters). |
| job | character | Job title (e.g. 'Stringer'). |
| job_code | character | Four-letter job type code (e.g. 'MSTR'). |
| title | character | Specific role title for the assignment. |
| person_id | integer | MLB person id for the datacaster. |
| person_full_name | character | Full name of the datacaster. |
| person_link | character | API relative link to the person. |

## Examples

``` r
# \donttest{
  try(mlb_jobs_datacasters(sport_id=1))
#> ── MLB Jobs Datacasters data from MLB.com ─────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:23:57 UTC
#> # A tibble: 214 × 7
#>    jersey_number job      job_code title    person_id person_full_name  
#>    <chr>         <chr>    <chr>    <chr>        <int> <chr>             
#>  1 ""            Stringer MSTR     Stringer    666938 Edward Alvarez    
#>  2 ""            Stringer MSTR     Stringer    684798 David Amoriello   
#>  3 ""            Stringer MSTR     Stringer    632811 Doug Anderson     
#>  4 ""            Stringer MSTR     Stringer    584273 Andy Andres       
#>  5 ""            Stringer MSTR     Stringer    511710 Troy Andre        
#>  6 ""            Stringer MSTR     Stringer    584288 Jose Isabel Angui…
#>  7 ""            Stringer MSTR     Stringer    800990 Terri Arms        
#>  8 ""            Stringer MSTR     Stringer    821942 Amanda Bachelder  
#>  9 ""            Stringer MSTR     Stringer    670060 Keith Barnes      
#> 10 ""            Stringer MSTR     Stringer    427022 Matt Bartlett     
#> # ℹ 204 more rows
#> # ℹ 1 more variable: person_link <chr>
# }
```

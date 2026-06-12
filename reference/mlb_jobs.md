# **MLB Jobs**

**MLB Jobs**

## Usage

``` r
mlb_jobs(job_type = "UMPR", sport_id = NULL, date = NULL)
```

## Arguments

- job_type:

  Return information for a given job_type. See
  [`mlb_job_types()`](https://billpetti.github.io/baseballr/reference/mlb_job_types.md)

- sport_id:

  Return information for a given sport_id.

- date:

  Return information for a given date.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| jersey_number | character | Jersey number worn (often blank for non-uniformed roles). |
| job | character | Job title (e.g. 'Umpire'). |
| job_code | character | Four-letter job type code (e.g. 'UMPR'). |
| title | character | Specific role title for the assignment. |
| person_id | integer | MLB person id for the individual. |
| person_full_name | character | Full name of the individual. |
| person_link | character | API relative link to the person. |

## Examples

``` r
# \donttest{
  try(mlb_jobs(job_type='UMPR'))
#> ── MLB Jobs data from MLB.com ─────────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:15:56 UTC
#> # A tibble: 98 × 7
#>    jersey_number job    job_code title  person_id person_full_name
#>    <chr>         <chr>  <chr>    <chr>      <int> <chr>           
#>  1 67            Umpire UMPR     Umpire    596809 Ryan Additon    
#>  2 12            Umpire UMPR     Umpire    623938 Erich Bacchus   
#>  3 70            Umpire UMPR     Umpire    545402 John Bacon      
#>  4 71            Umpire UMPR     Umpire    490319 Jordan Baker    
#>  5 41            Umpire UMPR     Umpire    665576 Brock Ballou    
#>  6 29            Umpire UMPR     Umpire    503493 Sean Barber     
#>  7 23            Umpire UMPR     Umpire    427013 Lance Barksdale 
#>  8 16            Umpire UMPR     Umpire    483561 Lance Barrett   
#>  9 87            Umpire UMPR     Umpire    482608 Scott Barry     
#> 10 38            Umpire UMPR     Umpire    644760 Adam Beck       
#> # ℹ 88 more rows
#> # ℹ 1 more variable: person_link <chr>
# }
```

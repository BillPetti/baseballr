# **MLB Jobs Official Scorers**

**MLB Jobs Official Scorers**

## Usage

``` r
mlb_jobs_official_scorers(sport_id = NULL, date = NULL)
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
| jersey_number | character | Jersey number (typically blank for official scorers). |
| job | character | Job title (e.g. 'Official Scorer'). |
| job_code | character | Four-letter job type code (e.g. 'SCOR'). |
| title | character | Specific role title for the assignment. |
| person_id | integer | MLB person id for the official scorer. |
| person_full_name | character | Full name of the official scorer. |
| person_link | character | API relative link to the person. |

## Examples

``` r
# \donttest{
  try(mlb_jobs_official_scorers(sport_id=1))
#> ── MLB Jobs Official Scorers data from MLB.com ────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 01:57:13 UTC
#> # A tibble: 236 × 7
#>    jersey_number job           job_code title person_id person_full_name
#>    <chr>         <chr>         <chr>    <chr>     <int> <chr>           
#>  1 ""            Official Sco… SCOR     Offi…    582413 Fernando Alcala 
#>  2 ""            Official Sco… SCOR     Offi…    434138 Billy Altman    
#>  3 ""            Official Sco… SCOR     Offi…    679924 Kenny Ayres     
#>  4 ""            Official Sco… SCOR     Offi…    839313 Zach Bamberger  
#>  5 ""            Official Sco… SCOR     Offi…    478601 Tyler Barton    
#>  6 ""            Official Sco… SCOR     Offi…    831878 Leslie Basler   
#>  7 ""            Official Sco… SCOR     Offi…    493719 Bob Beghtol     
#>  8 ""            Official Sco… SCOR     Offi…    679730 Devin Benson    
#>  9 ""            Official Sco… SCOR     Offi…    482136 Brian Berger    
#> 10 ""            Official Sco… SCOR     Offi…    550488 Court Berry-Tri…
#> # ℹ 226 more rows
#> # ℹ 1 more variable: person_link <chr>
# }
```

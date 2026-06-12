# **MLB Job Types**

**MLB Job Types**

## Usage

``` r
mlb_job_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| job_code | character | Four-letter job type code (e.g. 'UMPR', 'UDIR'). |
| job | character | Job title (e.g. 'Umpire', 'Director of Instant Replay'). |
| sort_order | integer | Display sort order for the job type. |

## Examples

``` r
# \donttest{
  try(mlb_job_types())
#> ── MLB Job Types data from MLB.com ────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:24:53 UTC
#> # A tibble: 393 × 3
#>    job_code job                        sort_order
#>    <chr>    <chr>                           <int>
#>  1 UMPR     Umpire                              1
#>  2 UDIR     Director of Instant Replay         11
#>  3 ROFF     Replay Official                    21
#>  4 MSTR     Stringer                          101
#>  5 BOSS     BOSS Operator                     111
#>  6 FTCO     Field Timing Coordinator          121
#>  7 VRMO     Video Room Monitor                131
#>  8 TRCK     Tracking Operator                 151
#>  9 SCRB     Scrubber Operator                 161
#> 10 PITO     Pitchcast Operator                171
#> # ℹ 383 more rows
# }
```

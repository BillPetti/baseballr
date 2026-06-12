# **MLB Baseball Stats**

**MLB Baseball Stats**

## Usage

``` r
mlb_baseball_stats()
```

## Value

Returns a tibble with the following columns:

|                   |           |                                                |
|-------------------|-----------|------------------------------------------------|
| col_name          | types     | description                                    |
| stat_name         | character | Internal stat name.                            |
| stat_lookup_param | character | Lookup parameter/abbreviation for the stat.    |
| is_counting       | logical   | Whether the stat is a counting stat.           |
| stat_label        | character | Human-readable stat label.                     |
| stat_group        | character | Stat group (e.g. hitting, pitching, fielding). |

## Examples

``` r
# \donttest{
  try(mlb_baseball_stats())
#> ── MLB Baseball Stats data from MLB.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:23:36 UTC
#> # A tibble: 324 × 5
#>    stat_name        stat_lookup_param is_counting stat_label stat_group
#>    <chr>            <chr>             <lgl>       <chr>      <chr>     
#>  1 airOuts          ao                TRUE        Airouts    pitching  
#>  2 assists          a                 TRUE        Assist     fielding  
#>  3 assists          a                 TRUE        Assist     catching  
#>  4 atBats           ab                TRUE        NA         hitting   
#>  5 atBats           ab                TRUE        NA         catching  
#>  6 atBatsPerHomeRun NA                FALSE       NA         hitting   
#>  7 balk             bk                TRUE        Balk       pitching  
#>  8 battingAverage   avg               FALSE       NA         hitting   
#>  9 battingAverage   avg               FALSE       NA         pitching  
#> 10 battingAverage   avg               FALSE       NA         catching  
#> # ℹ 314 more rows
# }
```

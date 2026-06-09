# **Scrape League Payroll Breakdowns from Spotrac**

This function allows you to scrape each team's payroll from Spotrac.

## Usage

``` r
sptrc_league_payrolls(year = most_recent_mlb_season())
```

## Arguments

- year:

  Year to load

## Value

A data frame of contract data.

|  |  |  |
|----|----|----|
| col_name | types | description |
| year | character | Payroll season. |
| team | character | Full team name. |
| team_abbr | character | Team abbreviation. |
| rank | numeric | League rank by total payroll allocations. |
| record | character | Team win-loss record for the season. |
| avg_age_team | character | Roster-weighted average age of the team. |
| total_payroll_allocations | numeric | Total payroll allocations across all roster statuses (USD). |
| active_26_man | numeric | Payroll allocated to the active 26-man roster (USD). |
| injured | numeric | Payroll allocated to players on the injured list (USD). |
| retained | numeric | Retained salary owed to players no longer on the roster (USD). |
| buried | numeric | Payroll for players assigned to the minors ("buried" contracts) (USD). |

Column names after `rank` mirror Spotrac's current league-payroll table
and may change as Spotrac updates its layout.

## Examples

``` r
# \donttest{
  try(sptrc_league_payrolls(year = most_recent_mlb_season()))
#> ── MLB Payroll data from Spotrac.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-09 20:45:05 UTC
#> # A tibble: 32 × 11
#>    year  team  team_abbr  rank record avg_age_team
#>    <chr> <chr> <chr>     <dbl> <chr>  <chr>       
#>  1 2026  NYM   NYM           1 29-36  29.3        
#>  2 2026  LAD   LAD           2 42-24  29.3        
#>  3 2026  NYY   NYY           3 39-26  30.1        
#>  4 2026  TOR   TOR           4 32-35  29.0        
#>  5 2026  PHI   PHI           5 36-30  30.0        
#>  6 2026  ATL   ATL           6 45-21  31.5        
#>  7 2026  HOU   HOU           7 31-37  28.7        
#>  8 2026  CHC   CHC           8 34-32  29.8        
#>  9 2026  SD    SD            9 34-31  29.2        
#> 10 2026  DET   DET          10 27-39  28.1        
#> # ℹ 22 more rows
#> # ℹ 5 more variables: total_payroll_allocations <dbl>,
#> #   active_26_man <dbl>, injured <dbl>, retained <dbl>, buried <dbl>
# }
```

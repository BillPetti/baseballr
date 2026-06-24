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
#> ℹ Data updated: 2026-06-24 02:06:45 UTC
#> # A tibble: 32 × 11
#>    year  team  team_abbr  rank record avg_age_team
#>    <chr> <chr> <chr>     <dbl> <chr>  <chr>       
#>  1 2026  NYM   NYM           1 34-43  29.4        
#>  2 2026  LAD   LAD           2 50-29  29.2        
#>  3 2026  NYY   NYY           3 46-31  29.6        
#>  4 2026  TOR   TOR           4 39-39  29.1        
#>  5 2026  PHI   PHI           5 42-36  29.7        
#>  6 2026  ATL   ATL           6 48-29  30.5        
#>  7 2026  HOU   HOU           7 37-43  28.6        
#>  8 2026  CHC   CHC           8 40-37  29.9        
#>  9 2026  SD    SD            9 40-37  29.4        
#> 10 2026  DET   DET          10 34-44  28.5        
#> # ℹ 22 more rows
#> # ℹ 5 more variables: total_payroll_allocations <dbl>,
#> #   active_26_man <dbl>, injured <dbl>, retained <dbl>, buried <dbl>
# }
```

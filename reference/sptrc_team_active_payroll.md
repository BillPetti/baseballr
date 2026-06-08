# **Scrape Team Active Payroll Breakdown from Spotrac**

This function allows you to scrape a team's active payroll from Spotrac.

## Usage

``` r
sptrc_team_active_payroll(team_abbr, year = most_recent_mlb_season())
```

## Arguments

- team_abbr:

  Team abbreviation

- year:

  Year to load

## Value

A data frame of contract data.

|  |  |  |
|----|----|----|
| col_name | types | description |
| year | numeric | Payroll season. |
| team | character | Team abbreviation supplied to the function. |
| player_name | character | Player name. |
| roster_status | character | Payroll table the row came from: Active, IL, or Retained Salary. |
| pos | character | Player position. |
| exp | character | Years of MLB service experience. |
| options_minor | character | Remaining minor-league option years. |
| status | character | Contract status (e.g. signed, arbitration, pre-arbitration). |
| payroll_salary | numeric | Salary counted against the team payroll (USD). |
| payroll_salary_adjusted | numeric | Payroll salary adjusted for prorated and retained amounts (USD). |
| base_salary | numeric | Player base salary for the season (USD). |
| signing_bonus | numeric | Signing bonus allocated to the season (USD). |
| waiver_options | character | Waiver and option flags reported by Spotrac. |

Columns after `roster_status` mirror Spotrac's current payroll table and
may change as Spotrac updates its layout.

## Examples

``` r
# \donttest{
 try(sptrc_team_active_payroll(team_abbr = "BAL", year = most_recent_mlb_season()))
#> ── MLB Active Payroll data from Spotrac.com ───────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 01:58:36 UTC
#> # A tibble: 38 × 18
#>     year team  player_name      roster_status pos   exp   options_minor
#>    <dbl> <chr> <chr>            <chr>         <chr> <chr> <chr>        
#>  1  2026 BAL   Pete Alonso      Active        1B    7     n/a          
#>  2  2026 BAL   Chris Bassitt    Active        SP    9.13  n/a          
#>  3  2026 BAL   Tyler O'Neill    Active        RF    7.059 n/a          
#>  4  2026 BAL   Taylor Ward      Active        LF    5.164 n/a          
#>  5  2026 BAL   Andrew Kittredge Active        RP    7.07  n/a          
#>  6  2026 BAL   Gunnar Henderson Active        SS    3.036 3            
#>  7  2026 BAL   Adley Rutschman  Active        C     4     3            
#>  8  2026 BAL   Trevor Rogers    Active        SP    5.047 n/a          
#>  9  2026 BAL   Kyle Bradish     Active        SP    3.16  2            
#> 10  2026 BAL   Keegan Akin      Active        RP    5.083 n/a          
#> # ℹ 28 more rows
#> # ℹ 11 more variables: status <chr>, payroll_salary <dbl>,
#> #   payroll_salary_adjusted <dbl>, base_salary <dbl>,
#> #   signing_bonus <dbl>, x <chr>, x_2 <chr>, x_3 <chr>, x_4 <chr>,
#> #   x_5 <chr>, waiver_options <chr>
# }
```

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
#> ✖ 2026-06-08 03:47:18.436064: Invalid arguments or no contract data available!
#> data frame with 0 columns and 0 rows
# }
```

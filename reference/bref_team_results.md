# **Scrape Team Results**

This function allows you to scrape schedule and results for a major
league team from Baseball-Reference.com

## Usage

``` r
bref_team_results(Tm, year)
```

## Arguments

- Tm:

  The abbreviation used by Baseball-Reference.com for the team whose
  results you want to scrape.

- year:

  Season for which you want to scrape the park factors.

## Value

Returns a tibble of MLB team results, one row per game on the team's
schedule, with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| Gm | character | Game number / label in the schedule (includes postseason labels). |
| Date | character | Day-of-week and calendar date of the game. |
| Tm | character | Team abbreviation. |
| H_A | character | Home/away indicator (A denotes a road game). |
| Opp | character | Opponent team abbreviation. |
| Result | character | Game outcome from the team's perspective (W/L). |
| R | integer | Runs scored by the team. |
| RA | integer | Runs allowed (opponent runs). |
| Inn | integer | Innings played when not nine (e.g. extra innings). |
| Record | character | Team's cumulative win-loss record after the game. |
| Rank | character | Team's standing/rank in its division after the game. |
| GB | character | Games behind the division leader after the game. |
| Win | character | Winning pitcher. |
| Loss | character | Losing pitcher. |
| Save | character | Pitcher credited with the save (N if none). |
| Time | character | Duration of the game. |
| D/N | character | Day (D) or night (N) game. |
| Attendance | numeric | Announced attendance. |
| cLI | numeric | Championship leverage index of the game. |
| Streak | character | Win/loss streak entering the game. |
| Orig_Scheduled | character | Original scheduled date (for rescheduled games). |
| Year | numeric | Season year. |

## Examples

``` r
# \donttest{
  try(bref_team_results("NYM", 2015))
#> ── MLB Team Results data from baseball-reference.com ───────────────────
#> ℹ Data updated: 2026-06-12 12:13:15 UTC
#> # A tibble: 162 × 22
#>       Gm Date    Tm    H_A   Opp   Result     R    RA Inn   Record  Rank
#>    <dbl> <chr>   <chr> <chr> <chr> <chr>  <dbl> <dbl> <chr> <chr>  <dbl>
#>  1     1 Monday… NYM   A     WSN   W          3     1 ""    1-0        1
#>  2     2 Wednes… NYM   A     WSN   L          1     2 ""    1-1        2
#>  3     3 Thursd… NYM   A     WSN   W          6     3 ""    2-1        2
#>  4     4 Friday… NYM   A     ATL   L          3     5 ""    2-2        2
#>  5     5 Saturd… NYM   A     ATL   L          3     5 ""    2-3        3
#>  6     6 Sunday… NYM   A     ATL   W          4     3 ""    3-3        2
#>  7     7 Monday… NYM   H     PHI   W          2     0 ""    4-3        2
#>  8     8 Tuesda… NYM   H     PHI   W          6     5 ""    5-3        2
#>  9     9 Wednes… NYM   H     PHI   W          6     1 ""    6-3        1
#> 10    10 Thursd… NYM   H     MIA   W          7     5 ""    7-3        1
#> # ℹ 152 more rows
#> # ℹ 11 more variables: GB <chr>, Win <chr>, Loss <chr>, Save <chr>,
#> #   Time <chr>, `D/N` <chr>, Attendance <dbl>, cLI <dbl>, Streak <dbl>,
#> #   Orig_Scheduled <chr>, Year <dbl>
  try(bref_team_results(Tm="TBR", year=2008))
#> ── MLB Team Results data from baseball-reference.com ───────────────────
#> ℹ Data updated: 2026-06-12 12:13:20 UTC
#> # A tibble: 162 × 22
#>       Gm Date    Tm    H_A   Opp   Result     R    RA Inn   Record  Rank
#>    <dbl> <chr>   <chr> <chr> <chr> <chr>  <dbl> <dbl> <chr> <chr>  <dbl>
#>  1     1 Monday… TBR   A     BAL   W          6     2 ""    1-0        1
#>  2     2 Wednes… TBR   A     BAL   L          6     9 ""    1-1        2
#>  3     3 Friday… TBR   A     NYY   W         13     4 ""    2-1        1
#>  4     4 Saturd… TBR   A     NYY   W          6     3 ""    3-1        1
#>  5     5 Sunday… TBR   A     NYY   L          0     2 ""    3-2        3
#>  6     6 Monday… TBR   A     NYY   L          1     6 ""    3-3        4
#>  7     7 Tuesda… TBR   H     SEA   L          5     6 ""    3-4        5
#>  8     8 Wednes… TBR   H     SEA   L          1     7 ""    3-5        5
#>  9     9 Thursd… TBR   H     SEA   W          7     0 ""    4-5        4
#> 10    10 Friday… TBR   H     BAL   W         10     5 ""    5-5        3
#> # ℹ 152 more rows
#> # ℹ 11 more variables: GB <chr>, Win <chr>, Loss <chr>, Save <chr>,
#> #   Time <chr>, `D/N` <chr>, Attendance <dbl>, cLI <dbl>, Streak <dbl>,
#> #   Orig_Scheduled <chr>, Year <dbl>
# }
```

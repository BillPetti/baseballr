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

## Examples

``` r
# \donttest{
  try(bref_team_results("NYM", 2015))
#> Warning: Unknown or uninitialised column: `Attendance`.
#> ✖ 2026-06-08 01:55:17.991137: Invalid arguments or no team results data available!
#> # A tibble: 14 × 16
#>    Gm      Date  Tm    H_A   Opp   Result     R    RA   Inn Record Rank 
#>    <chr>   <chr> <chr> <chr> <chr> <chr>  <int> <int> <int> <chr>  <chr>
#>  1 NLDS G… Frid… NYM   A     LAD   W          3     1    NA 1-0    deGr…
#>  2 NLDS G… Satu… NYM   A     LAD   L          2     5    NA 1-1    Grei…
#>  3 NLDS G… Mond… NYM   H     LAD   W         13     7    NA 2-1    Harv…
#>  4 NLDS G… Tues… NYM   H     LAD   L          1     3    NA 2-2    Kers…
#>  5 NLDS G… Thur… NYM   A     LAD   W          3     2    NA 3-2    deGr…
#>  6 NLCS G… Satu… NYM   H     CHC   W          4     2    NA 4-2    Harv…
#>  7 NLCS G… Sund… NYM   H     CHC   W          4     1    NA 5-2    Synd…
#>  8 NLCS G… Tues… NYM   A     CHC   W          5     2    NA 6-2    deGr…
#>  9 NLCS G… Wedn… NYM   A     CHC   W          8     3    NA 7-2    Colón
#> 10 WS Gam… Tues… NYM   A     KCR   L          4     5    14 7-3    Young
#> 11 WS Gam… Wedn… NYM   A     KCR   L          1     7    NA 7-4    Cueto
#> 12 WS Gam… Frid… NYM   H     KCR   W          9     3    NA 8-4    Synd…
#> 13 WS Gam… Satu… NYM   H     KCR   L          3     5    NA 8-5    Mads…
#> 14 WS Gam… Sund… NYM   H     KCR   L          2     7    12 8-6    Hoch…
#> # ℹ 5 more variables: GB <chr>, Win <chr>, Loss <chr>, Save <chr>,
#> #   Time <chr>
  try(bref_team_results(Tm="TBR", year=2008))
#> Warning: Unknown or uninitialised column: `Attendance`.
#> ✖ 2026-06-08 01:55:18.151907: Invalid arguments or no team results data available!
#> # A tibble: 16 × 16
#>    Gm      Date  Tm    H_A   Opp   Result     R    RA   Inn Record Rank 
#>    <chr>   <chr> <chr> <chr> <chr> <chr>  <int> <int> <int> <chr>  <chr>
#>  1 ALDS G… Thur… TBR   H     CHW   W          6     4    NA 1-0    Shie…
#>  2 ALDS G… Frid… TBR   H     CHW   W          6     2    NA 2-0    Kazm…
#>  3 ALDS G… Sund… TBR   A     CHW   L          3     5    NA 2-1    Danks
#>  4 ALDS G… Mond… TBR   A     CHW   W          6     2    NA 3-1    Sonn…
#>  5 ALCS G… Frid… TBR   H     BOS   L          0     2    NA 3-2    Mats…
#>  6 ALCS G… Satu… TBR   H     BOS   W          9     8    11 4-2    Price
#>  7 ALCS G… Mond… TBR   A     BOS   W          9     1    NA 5-2    Garza
#>  8 ALCS G… Tues… TBR   A     BOS   W         13     4    NA 6-2    Sonn…
#>  9 ALCS G… Thur… TBR   A     BOS   L          7     8    NA 6-3    Mast…
#> 10 ALCS G… Satu… TBR   H     BOS   L          2     4    NA 6-4    Beck…
#> 11 ALCS G… Sund… TBR   H     BOS   W          3     1    NA 7-4    Garza
#> 12 WS Gam… Wedn… TBR   H     PHI   L          2     3    NA 7-5    Hame…
#> 13 WS Gam… Thur… TBR   H     PHI   W          4     2    NA 8-5    Shie…
#> 14 WS Gam… Satu… TBR   A     PHI   L          4     5    NA 8-6    Rome…
#> 15 WS Gam… Sund… TBR   A     PHI   L          2    10    NA 8-7    Blan…
#> 16 WS Gam… Mond… TBR   A     PHI   L          3     4    NA 8-8    Rome…
#> # ℹ 5 more variables: GB <chr>, Win <chr>, Loss <chr>, Save <chr>,
#> #   Time <chr>
# }
```

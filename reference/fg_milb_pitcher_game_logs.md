# **Scrape MiLB game logs for pitchers from FanGraphs**

This function allows you to scrape MiLB game logs for individual batters
from FanGraphs.com.

## Usage

``` r
fg_milb_pitcher_game_logs(playerid, year)
```

## Arguments

- playerid:

  The pitcher's minor league ID from FanGraphs.com.

- year:

  The season for which game logs should be returned.

## Value

Returns a tibble of Minor League pitcher game logs, one row per game,
with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| Date | character | Calendar date of the game (YYYY-MM-DD). |
| Team | character | Minor league team the pitcher played for. |
| Level | character | Minor league classification level (e.g. (AA), (AAA)). |
| Opp | character | Opponent team; leading @ indicates a road game. |
| W | numeric | Wins. |
| L | numeric | Losses. |
| ERA | numeric | Earned run average (per 9 innings). |
| G | numeric | Games pitched (1 per row). |
| GS | numeric | Games started. |
| QS | numeric | Quality starts (6+ IP, 3 or fewer earned runs). |
| CG | numeric | Complete games. |
| ShO | numeric | Shutouts. |
| SV | numeric | Saves. |
| IP | numeric | Innings pitched. |
| TBF | numeric | Total batters faced. |
| H | numeric | Hits allowed. |
| R | numeric | Runs allowed. |
| ER | numeric | Earned runs allowed. |
| HR | numeric | Home runs allowed. |
| BB | numeric | Walks allowed. |
| IBB | numeric | Intentional walks allowed. |
| HBP | numeric | Batters hit by pitch. |
| WP | numeric | Wild pitches. |
| BK | numeric | Balks. |
| SO | numeric | Strikeouts. |
| K/9 | numeric | Strikeouts per 9 innings. |
| BB/9 | numeric | Walks per 9 innings. |
| K/BB | numeric | Strikeout-to-walk ratio. |
| HR/9 | numeric | Home runs allowed per 9 innings. |
| K% | numeric | Strikeout rate (per batter faced). |
| K-BB% | numeric | Strikeout rate minus walk rate. |
| BB% | numeric | Walk rate (per batter faced). |
| AVG | numeric | Opponent batting average allowed. |
| WHIP | numeric | Walks plus hits per inning pitched. |
| BABIP | numeric | Batting average on balls in play allowed. |
| LOB% | numeric | Left-on-base percentage (strand rate). |
| FIP | numeric | Fielding independent pitching. |
| ERA- | numeric | ERA scaled to league/park (100 = average, lower better). |
| FIP- | numeric | FIP scaled to league/park (100 = average, lower better). |
| xFIP- | numeric | Expected FIP scaled to league/park (100 = average). |
| gamedate | character | Game date as parsed from the source feed. |
| dh | integer | Doubleheader game indicator (0 = single game). |

## Examples

``` r
# \donttest{
  try(fg_milb_pitcher_game_logs(playerid = "sa3020682", year=2023))
#> ✖ 2026-06-08 03:45:05.640487: Invalid arguments or no MiLB pitcher game logs data available!
#>          Date Team Level  Opp W L       ERA G GS QS CG ShO SV  IP TBF H
#> 2  2023-08-22  NYY  (AA)  COL 0 0  0.000000 1  1  0  0   0  0 5.0  19 4
#> 3  2023-08-15  NYY  (AA) @PHI 1 0  3.000000 1  1  1  0   0  0 6.0  23 6
#> 4  2023-08-08  NYY  (AA)  NYM 0 1 11.250000 1  1  0  0   0  0 4.0  18 4
#> 5  2023-08-02  NYY  (AA) @CLE 0 1  3.600000 1  1  0  0   0  0 5.0  24 5
#> 6  2023-07-26  NYY  (AA)  BAL 0 0  7.200000 1  1  0  0   0  0 5.0  24 4
#> 7  2023-07-20  NYY  (AA) @COL 0 0  5.142857 1  1  0  0   0  0 7.0  28 9
#> 8  2023-07-14  NYY  (AA) @NYM 1 0  3.375002 1  1  0  0   0  0 5.1  22 3
#> 9  2023-07-06  NYY  (AA)  PHI 0 0  3.600000 1  1  0  0   0  0 5.0  21 3
#> 10 2023-07-01  NYY  (AA) @BAL 0 0  7.941172 1  1  0  0   0  0 5.2  25 8
#> 11 2023-06-24  NYY  (AA)  TOR 0 0  1.588234 1  1  0  0   0  0 5.2  21 4
#> 12 2023-06-18  NYY  (AA)  PIT 0 0  3.000000 1  1  1  0   0  0 6.0  23 4
#> 13 2023-06-10  NYY  (A+) @PHI 1 0  0.000000 1  1  0  0   0  0 5.2  19 1
#> 14 2023-06-03  NYY  (A+)  BAL 0 0  2.571429 1  1  1  0   0  0 7.0  25 3
#> 15 2023-05-27  NYY  (A+) @WSN 0 0  3.375002 1  1  0  0   0  0 5.1  21 3
#> 16 2023-05-21  NYY  (A+)  NYM 0 1  3.375002 1  1  0  0   0  0 5.1  24 5
#> 17 2023-05-13  NYY  (A+) @BAL 1 0  0.000000 1  1  0  0   0  0 5.0  19 3
#> 18 2023-05-06  NYY  (A+)  WSN 0 0  1.800000 1  1  0  0   0  0 5.0  21 4
#> 19 2023-04-25  NYY  (A+) @BOS 0 0  9.642851 1  1  0  0   0  0 4.2  24 6
#> 20 2023-04-18  NYY  (A+) @ATL 0 0  3.857140 1  1  0  0   0  0 4.2  18 3
#> 21 2023-04-11  NYY  (A+)  BAL 0 0  0.000000 1  1  0  0   0  0 4.1  19 3
#>    R ER HR BB IBB HBP WP BK SO       K/9     BB/9     K/BB     HR/9
#> 2  0  0  0  1   0   0  2  0  6 10.800000 1.800000 6.000000 0.000000
#> 3  3  2  1  0   0   0  0  0  6  9.000000 0.000000 6.000000 1.500000
#> 4  5  5  0  3   0   0  0  0  4  9.000000 6.750000 1.333333 0.000000
#> 5  4  2  1  3   0   0  1  1  4  7.200000 5.400000 1.333333 1.800000
#> 6  4  4  1  5   0   0  0  0  8 14.400000 9.000000 1.600000 1.800000
#> 7  4  4  3  0   0   0  0  0  7  9.000000 0.000000 7.000000 3.857143
#> 8  2  2  0  3   0   0  1  0  7 11.812507 5.062503 2.333333 0.000000
#> 9  2  2  1  3   0   0  1  0 10 18.000000 5.400000 3.333333 1.800000
#> 10 5  5  1  1   0   0  0  1  3  4.764703 1.588234 3.000000 1.588234
#> 11 1  1  0  2   0   0  0  0  5  7.941172 3.176469 2.500000 0.000000
#> 12 3  2  0  0   0   0  0  0  8 12.000000 0.000000 8.000000 0.000000
#> 13 0  0  0  1   0   0  0  0  8 12.705875 1.588234 8.000000 0.000000
#> 14 2  2  0  2   0   0  0  0 10 12.857143 2.571429 5.000000 0.000000
#> 15 2  2  2  2   0   0  0  0 10 16.875010 3.375002 5.000000 3.375002
#> 16 4  2  1  2   0   0  1  0  7 11.812507 3.375002 3.500000 1.687501
#> 17 2  0  0  1   0   0  1  0  9 16.200000 1.800000 9.000000 0.000000
#> 18 1  1  0  1   0   0  1  0  9 16.200000 1.800000 9.000000 0.000000
#> 19 5  5  1  3   0   0  2  0 10 19.285701 5.785710 3.333333 1.928570
#> 20 2  2  1  1   0   0  0  0  6 11.571421 1.928570 6.000000 1.928570
#> 21 0  0  0  3   0   0  0  0  8 16.615397 6.230774 2.666667 0.000000
#>           K%      K-BB%        BB%        AVG      WHIP     BABIP
#> 2  0.3157895 0.26315790 0.05263158 0.22222222 1.0000000 0.3333333
#> 3  0.2608696 0.26086957 0.00000000 0.26086957 1.0000000 0.3125000
#> 4  0.2222222 0.05555556 0.16666667 0.26666667 1.7500000 0.3636364
#> 5  0.1666667 0.04166667 0.12500000 0.23809524 1.6000000 0.2500000
#> 6  0.3333333 0.12500000 0.20833333 0.21052632 1.8000000 0.3000000
#> 7  0.2500000 0.25000000 0.00000000 0.32142857 1.2857143 0.3333333
#> 8  0.3181818 0.18181818 0.13636364 0.15789474 1.1250007 0.2500000
#> 9  0.4761905 0.33333333 0.14285714 0.16666667 1.2000000 0.2857143
#> 10 0.1200000 0.08000000 0.04000000 0.33333333 1.5882344 0.3500000
#> 11 0.2380952 0.14285714 0.09523810 0.21052632 1.0588229 0.2857143
#> 12 0.3478261 0.34782609 0.00000000 0.17391304 0.6666667 0.2666667
#> 13 0.4210526 0.36842105 0.05263158 0.05555556 0.3529410 0.1000000
#> 14 0.4000000 0.32000000 0.08000000 0.13043478 0.7142857 0.2307692
#> 15 0.4761905 0.38095238 0.09523810 0.15789474 0.9375006 0.1428571
#> 16 0.2916667 0.20833333 0.08333333 0.22727273 1.3125008 0.2857143
#> 17 0.4736842 0.42105263 0.05263158 0.16666667 0.8000000 0.3333333
#> 18 0.4285714 0.38095238 0.04761905 0.20000000 1.0000000 0.3636364
#> 19 0.4166667 0.29166667 0.12500000 0.28571429 1.9285701 0.5000000
#> 20 0.3333333 0.27777778 0.05555556 0.17647059 0.8571423 0.2000000
#> 21 0.4210526 0.26315790 0.15789474 0.18750000 1.3846164 0.3750000
#>         LOB%       FIP      ERA-      FIP-     xFIP-   gamedate dh
#> 2  1.0000000 1.4756021   0.00000  35.22497  75.11730 2023-08-22  1
#> 3  0.6521739 3.4422687  73.10137  82.17244  63.67334 2023-08-15  1
#> 4  0.2857143 3.5256021 274.13014  84.16174 116.36000 2023-08-08  1
#> 5  0.6060606 6.0756021  87.72165 145.03429 142.23196 2023-08-02  1
#> 6  0.6578947 5.6756021 175.44329 135.48566 112.58336 2023-07-26  1
#> 7  1.0000000 6.8470306 125.31664 163.44952  87.81868 2023-07-20  1
#> 8  0.6666667 2.3381015  82.23909  55.81421  73.76413 2023-07-14  1
#> 9  0.8695652 3.6756021  87.72165  87.74247  58.99009 2023-07-06  1
#> 10 0.5263158 5.0403069 193.50352 120.32015 106.06281 2023-07-01  1
#> 11 0.8333333 2.5697201  38.70070  61.34331 113.86115 2023-06-24  1
#> 12 0.2500000 0.6089354  73.10137  14.53626  53.69640 2023-06-18  1
#> 13 1.0000000 1.4375945   0.00000  32.40906  71.36426 2023-06-10  1
#> 14 0.6000000 1.7317109  58.78531  39.03961  66.02395 2023-06-03  1
#> 15 1.0000000 5.9817122  77.15576 134.85143  42.65994 2023-05-27  1
#> 16 0.5357143 4.6692114  77.15576 105.26248  79.80464 2023-05-21  1
#> 17 0.5000000 0.7317109   0.00000  16.49565  35.41468 2023-05-13  1
#> 18 0.8000000 0.7317109  41.14972  16.49565  48.05779 2023-05-06  1
#> 19 0.5263158 4.1602820 220.44476  93.78920  64.76793 2023-04-25  1
#> 20 0.7692308 4.5888531  88.17790 103.45088  67.62979 2023-04-18  1
#> 21 1.0000000 2.1163251   0.00000  47.71033  76.75473 2023-04-11  1
# }
```

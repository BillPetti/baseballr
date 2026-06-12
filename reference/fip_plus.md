# **Calculate FIP and related metrics for any set of data**

This function allows you to calculate FIP and related metrics for any
given set of data, provided the right variables are in the data set. The
function currently returns both FIP per inning pitched, wOBA against
(based on batters faced), and wOBA against per instance of fair contact.

## Usage

``` r
fip_plus(df)
```

## Arguments

- df:

  A data frame of statistics that includes, at a minimum, the following
  columns: IP (innings pitched), BF (batters faced), uBB (unintentional
  walks), HBP (Hit By Pitch), x1B (singles), x2B (doubles), x3B
  (triples), HR (home runs), AB (at-bats), SH (sacrifice hits), SO
  (strike outs), and season.

## Value

Returns a tibble with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| bbref_id | character | Baseball-Reference player id |
| season | integer | Season (4-digit year) |
| Name | character | Player name |
| Age | numeric | Player age |
| Level | character | Level of play (e.g. MLB or minor-league level) |
| Team | character | Team name |
| G | numeric | Games |
| GS | numeric | Games started |
| W | numeric | Wins |
| L | numeric | Losses |
| SV | numeric | Saves |
| IP | numeric | Innings pitched |
| H | numeric | Hits allowed |
| R | numeric | Runs allowed |
| ER | numeric | Earned runs allowed |
| uBB | numeric | Unintentional walks allowed |
| BB | numeric | Walks (bases on balls) allowed |
| SO | numeric | Strikeouts |
| HR | numeric | Home runs allowed |
| HBP | numeric | Hit batters |
| ERA | numeric | Earned run average |
| AB | numeric | At-bats against |
| X1B | numeric | Singles allowed |
| X2B | numeric | Doubles allowed |
| X3B | numeric | Triples allowed |
| IBB | numeric | Intentional walks allowed |
| GDP | numeric | Grounded into double plays induced |
| SF | numeric | Sacrifice flies allowed |
| SB | numeric | Stolen bases allowed |
| CS | numeric | Caught stealing |
| PO | numeric | Pickoffs |
| BF | numeric | Batters faced |
| Pit | numeric | Pitches thrown |
| Str | numeric | Strikes thrown |
| StL | numeric | Strikes looking (called) |
| StS | numeric | Strikes swinging (whiffs) |
| GB.FB | numeric | Ground ball to fly ball ratio |
| LD | numeric | Line drives allowed |
| PU | numeric | Pop ups (infield fly) induced |
| WHIP | numeric | Walks plus hits per inning pitched |
| BAbip | numeric | Batting average on balls in play allowed |
| SO9 | numeric | Strikeouts per nine innings |
| SO.W | numeric | Strikeout-to-walk ratio |
| SO_perc | numeric | Strikeout percentage |
| uBB_perc | numeric | Unintentional walk percentage |
| SO_uBB | numeric | Strikeout percentage minus unintentional walk percentage |
| FIP | numeric | Fielding independent pitching |
| wOBA_against | numeric | Weighted on-base average against (based on batters faced) |
| wOBA_CON_against | numeric | Weighted on-base average against per instance of fair contact |

## Examples

``` r
# \donttest{
  try({
    df <- bref_daily_pitcher("2015-04-05", "2015-04-30")
    fip_plus(df)
  })
#> ── MLB Daily Pitcher data from baseball-reference.com ──────────────────
#> ℹ Data updated: 2026-06-12 12:24:12 UTC
#> # A tibble: 453 × 49
#>    bbref_id season Name    Age Level Team      G    GS     W     L    SV
#>    <chr>     <int> <chr> <dbl> <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 hembrhe…   2015 Heat…    26 Maj-… Bost…     1     0    NA    NA    NA
#>  2 hollade…   2015 Dere…    28 Maj-… Texas     1     1    NA     1    NA
#>  3 edwarjo…   2015 Jon …    27 Maj-… Texas     1     0    NA    NA    NA
#>  4 mazzoco…   2015 Cory…    25 Maj-… San …     2     0    NA    NA    NA
#>  5 gonzase…   2015 Seve…    22 Maj-… Phil…     1     1    NA     1    NA
#>  6 cornejo…   2015 John…    26 Maj-… Atla…     1     0    NA    NA    NA
#>  7 rearich…   2015 Chri…    27 Maj-… San …     5     0    NA    NA    NA
#>  8 vealdo01   2015 Donn…    30 Maj-… Atla…     1     0    NA    NA    NA
#>  9 rondojo…   2015 Jorg…    27 Maj-… Colo…     1     0    NA    NA    NA
#> 10 ranauan…   2015 Anth…    25 Maj-… Texas     1     1    NA     1    NA
#> # ℹ 443 more rows
#> # ℹ 38 more variables: IP <dbl>, H <dbl>, R <dbl>, ER <dbl>, uBB <dbl>,
#> #   BB <dbl>, SO <dbl>, HR <dbl>, HBP <dbl>, ERA <dbl>, AB <dbl>,
#> #   X1B <dbl>, X2B <dbl>, X3B <dbl>, IBB <dbl>, GDP <dbl>, SF <dbl>,
#> #   SB <dbl>, CS <dbl>, PO <dbl>, BF <dbl>, Pit <dbl>, Str <dbl>,
#> #   StL <dbl>, StS <dbl>, GB.FB <dbl>, LD <dbl>, PU <dbl>, WHIP <dbl>,
#> #   BAbip <dbl>, SO9 <dbl>, SO.W <dbl>, SO_perc <dbl>, …
# }
```

# **Scrape Pitcher Performance Data Over a Custom Time Frame**

This function allows you to scrape basic pitcher statistics over a
custom time frame. Data is sourced from Baseball-Reference.com.

## Usage

``` r
bref_daily_pitcher(t1, t2)
```

## Arguments

- t1:

  First date data should be scraped from. Should take the form
  "YEAR-MONTH-DAY"

- t2:

  Last date data should be scraped from. Should take the form
  "YEAR-MONTH-DAY"

## Value

Returns a tibble of pitcher performance over the requested date range,
one row per player, with the following columns:

|          |           |                                             |
|----------|-----------|---------------------------------------------|
| col_name | types     | description                                 |
| bbref_id | character | Baseball-Reference player id (slug).        |
| season   | integer   | Season year.                                |
| Name     | character | Player name.                                |
| Age      | numeric   | Player age during the season.               |
| Level    | character | League level (e.g. Maj-AL, Maj-NL).         |
| Team     | character | Team name.                                  |
| G        | numeric   | Games pitched.                              |
| GS       | numeric   | Games started.                              |
| W        | numeric   | Wins.                                       |
| L        | numeric   | Losses.                                     |
| SV       | numeric   | Saves.                                      |
| IP       | numeric   | Innings pitched.                            |
| H        | numeric   | Hits allowed.                               |
| R        | numeric   | Runs allowed.                               |
| ER       | numeric   | Earned runs allowed.                        |
| uBB      | numeric   | Unintentional walks allowed.                |
| BB       | numeric   | Walks allowed.                              |
| SO       | numeric   | Strikeouts.                                 |
| HR       | numeric   | Home runs allowed.                          |
| HBP      | numeric   | Batters hit by pitch.                       |
| ERA      | numeric   | Earned run average (per 9 innings).         |
| AB       | numeric   | At-bats against.                            |
| X1B      | numeric   | Singles allowed.                            |
| X2B      | numeric   | Doubles allowed.                            |
| X3B      | numeric   | Triples allowed.                            |
| IBB      | numeric   | Intentional walks allowed.                  |
| GDP      | numeric   | Double plays induced.                       |
| SF       | numeric   | Sacrifice flies allowed.                    |
| SB       | numeric   | Stolen bases allowed.                       |
| CS       | numeric   | Runners caught stealing.                    |
| PO       | numeric   | Pickoffs.                                   |
| BF       | numeric   | Batters faced.                              |
| Pit      | numeric   | Total pitches thrown.                       |
| Str      | numeric   | Strikes thrown (as a share of pitches).     |
| StL      | numeric   | Looking (called) strikes share.             |
| StS      | numeric   | Swinging strikes share.                     |
| GB.FB    | numeric   | Ground ball to fly ball share.              |
| LD       | numeric   | Line drive share.                           |
| PU       | numeric   | Pop-up (infield fly) share.                 |
| WHIP     | numeric   | Walks plus hits per inning pitched.         |
| BAbip    | numeric   | Batting average on balls in play allowed.   |
| SO9      | numeric   | Strikeouts per 9 innings.                   |
| SO.W     | numeric   | Strikeout-to-walk ratio.                    |
| SO_perc  | numeric   | Strikeout rate (per batter faced).          |
| uBB_perc | numeric   | Unintentional walk rate (per batter faced). |
| SO_uBB   | numeric   | Strikeouts minus unintentional walks.       |

## Examples

``` r
# \donttest{
  try(bref_daily_pitcher("2015-05-10", "2015-06-20"))
#> ── MLB Daily Pitcher data from baseball-reference.com ──────────────────
#> ℹ Data updated: 2026-06-12 12:37:48 UTC
#> # A tibble: 511 × 46
#>    bbref_id season Name    Age Level Team      G    GS     W     L    SV
#>    <chr>     <int> <chr> <dbl> <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 salech01   2015 Chri…    26 Maj-… Chic…     8     8     4     2    NA
#>  2 scherma…   2015 Max …    30 Maj-… Wash…     8     8     6     2    NA
#>  3 buehrma…   2015 Mark…    36 Maj-… Toro…     8     8     3     2    NA
#>  4 klubeco…   2015 Core…    29 Maj-… Clev…     8     8     3     4    NA
#>  5 keuchda…   2015 Dall…    27 Maj-… Hous…     8     8     4     3    NA
#>  6 degroja…   2015 Jaco…    27 Maj-… New …     8     8     4     2    NA
#>  7 samarje…   2015 Jeff…    30 Maj-… Chic…     8     8     2     2    NA
#>  8 colege01   2015 Gerr…    24 Maj-… Pitt…     8     8     7     1    NA
#>  9 buchhcl…   2015 Clay…    30 Maj-… Bost…     8     8     3     2    NA
#> 10 burnea.…   2015 A.J.…    38 Maj-… Pitt…     8     8     5     2    NA
#> # ℹ 501 more rows
#> # ℹ 35 more variables: IP <dbl>, H <dbl>, R <dbl>, ER <dbl>, uBB <dbl>,
#> #   BB <dbl>, SO <dbl>, HR <dbl>, HBP <dbl>, ERA <dbl>, AB <dbl>,
#> #   X1B <dbl>, X2B <dbl>, X3B <dbl>, IBB <dbl>, GDP <dbl>, SF <dbl>,
#> #   SB <dbl>, CS <dbl>, PO <dbl>, BF <dbl>, Pit <dbl>, Str <dbl>,
#> #   StL <dbl>, StS <dbl>, GB.FB <dbl>, LD <dbl>, PU <dbl>, WHIP <dbl>,
#> #   BAbip <dbl>, SO9 <dbl>, SO.W <dbl>, SO_perc <dbl>, …
# }
```

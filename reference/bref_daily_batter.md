# **Scrape Batter Performance Data Over a Custom Time Frame**

This function allows you to scrape basic batter statistics over a custom
time frame. Data is sourced from Baseball-Reference.com.

## Usage

``` r
bref_daily_batter(t1, t2)
```

## Arguments

- t1:

  First date data should be scraped from. Should take the form
  "YEAR-MONTH-DAY"

- t2:

  Last date data should be scraped from. Should take the form
  "YEAR-MONTH-DAY"

## Value

Returns a tibble of batter performance over the requested date range,
one row per player, with the following columns:

|          |           |                                      |
|----------|-----------|--------------------------------------|
| col_name | types     | description                          |
| bbref_id | character | Baseball-Reference player id (slug). |
| season   | integer   | Season year.                         |
| Name     | character | Player name.                         |
| Age      | numeric   | Player age during the season.        |
| Level    | character | League level (e.g. Maj-AL, Maj-NL).  |
| Team     | character | Team name.                           |
| G        | numeric   | Games played.                        |
| PA       | numeric   | Plate appearances.                   |
| AB       | numeric   | At-bats.                             |
| R        | numeric   | Runs scored.                         |
| H        | numeric   | Hits.                                |
| X1B      | numeric   | Singles.                             |
| X2B      | numeric   | Doubles.                             |
| X3B      | numeric   | Triples.                             |
| HR       | numeric   | Home runs.                           |
| RBI      | numeric   | Runs batted in.                      |
| BB       | numeric   | Walks (bases on balls).              |
| IBB      | numeric   | Intentional walks.                   |
| uBB      | numeric   | Unintentional walks.                 |
| SO       | numeric   | Strikeouts.                          |
| HBP      | numeric   | Times hit by pitch.                  |
| SH       | numeric   | Sacrifice hits (bunts).              |
| SF       | numeric   | Sacrifice flies.                     |
| GDP      | numeric   | Grounded into double plays.          |
| SB       | numeric   | Stolen bases.                        |
| CS       | numeric   | Times caught stealing.               |
| BA       | numeric   | Batting average.                     |
| OBP      | numeric   | On-base percentage.                  |
| SLG      | numeric   | Slugging percentage.                 |
| OPS      | numeric   | On-base plus slugging.               |

## Examples

``` r
# \donttest{
  try(bref_daily_batter(t1="2015-05-10", t2="2015-06-20"))
#> ── MLB Daily Batter data from baseball-reference.com ───────────────────
#> ℹ Data updated: 2026-06-12 03:16:32 UTC
#> # A tibble: 658 × 30
#>    bbref_id season Name    Age Level Team      G    PA    AB     R     H
#>    <chr>     <int> <chr> <dbl> <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 machama…   2015 Mann…    22 Maj-… Balt…    40   175   163    30    51
#>  2 blackch…   2015 Char…    28 Maj-… Colo…    40   175   153    24    37
#>  3 kipnija…   2015 Jaso…    28 Maj-… Clev…    37   174   143    31    57
#>  4 donaljo…   2015 Josh…    29 Maj-… Toro…    39   174   155    30    45
#>  5 burnsbi…   2015 Bill…    25 Maj-… Oakl…    37   172   159    25    52
#>  6 markani…   2015 Nick…    31 Maj-… Atla…    39   172   148    14    44
#>  7 choosh01   2015 Shin…    32 Maj-… Texas    38   172   155    24    42
#>  8 gordode…   2015 Dee …    27 Maj-… Miami    38   172   168    19    49
#>  9 fowlede…   2015 Dext…    29 Maj-… Chic…    37   170   148    28    32
#> 10 goldspa…   2015 Paul…    27 Maj-… Ariz…    39   169   133    26    48
#> # ℹ 648 more rows
#> # ℹ 19 more variables: X1B <dbl>, X2B <dbl>, X3B <dbl>, HR <dbl>,
#> #   RBI <dbl>, BB <dbl>, IBB <dbl>, uBB <dbl>, SO <dbl>, HBP <dbl>,
#> #   SH <dbl>, SF <dbl>, GDP <dbl>, SB <dbl>, CS <dbl>, BA <dbl>,
#> #   OBP <dbl>, SLG <dbl>, OPS <dbl>
# }
```

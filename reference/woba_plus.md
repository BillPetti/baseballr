# **Calculate wOBA and related metrics for any set of data**

This function allows you to calculate wOBA for any given set of data,
provided the right variables are in the data set. The function currently
returns both wOBA per plate appearance on wOBA per instance of fair
contact.

## Usage

``` r
woba_plus(df)
```

## Arguments

- df:

  A data frame of statistics that includes, at a minimum, the following
  columns: uBB (unintentional walks), HBP (Hit By Pitch), X1B (singles),
  X2B (doubles), X3B (triples), HR (home runs), AB (at-bats), SH
  (sacrifice hits), SO (strike outs), and season.

## Value

Returns a tibble with the wOBA factors calculated and the following
columns:

|          |           |                                                       |
|----------|-----------|-------------------------------------------------------|
| col_name | types     | description                                           |
| bbref_id | character | Baseball-Reference player id                          |
| season   | integer   | Season (4-digit year)                                 |
| Name     | character | Player name                                           |
| Age      | numeric   | Player age                                            |
| Level    | character | Level of play (e.g. MLB or minor-league level)        |
| Team     | character | Team name                                             |
| G        | numeric   | Games played                                          |
| PA       | numeric   | Plate appearances                                     |
| AB       | numeric   | At-bats                                               |
| R        | numeric   | Runs scored                                           |
| H        | numeric   | Hits                                                  |
| X1B      | numeric   | Singles                                               |
| X2B      | numeric   | Doubles                                               |
| X3B      | numeric   | Triples                                               |
| HR       | numeric   | Home runs                                             |
| RBI      | numeric   | Runs batted in                                        |
| BB       | numeric   | Walks (bases on balls)                                |
| IBB      | numeric   | Intentional walks                                     |
| uBB      | numeric   | Unintentional walks (BB minus IBB)                    |
| SO       | numeric   | Strikeouts                                            |
| HBP      | numeric   | Hit by pitch                                          |
| SH       | numeric   | Sacrifice hits (bunts)                                |
| SF       | numeric   | Sacrifice flies                                       |
| GDP      | numeric   | Grounded into double plays                            |
| SB       | numeric   | Stolen bases                                          |
| CS       | numeric   | Caught stealing                                       |
| BA       | numeric   | Batting average                                       |
| OBP      | numeric   | On-base percentage                                    |
| SLG      | numeric   | Slugging percentage                                   |
| OPS      | numeric   | On-base plus slugging                                 |
| wOBA     | numeric   | Weighted on-base average (per plate appearance)       |
| wOBA_CON | numeric   | Weighted on-base average per instance of fair contact |

## Examples

``` r
# \donttest{
 try({
   df <- bref_daily_batter("2015-08-01", "2015-10-03") 
   woba_plus(df)
 })
#> ── MLB Daily Batter data from baseball-reference.com ───────────────────
#> ℹ Data updated: 2026-06-12 11:57:39 UTC
#> # A tibble: 764 × 32
#>    bbref_id season Name    Age Level Team      G    PA    AB     R     H
#>    <chr>     <int> <chr> <dbl> <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 pindebr…   2015 Bran…    26 Maj-… New …     1     1     1     0     1
#>  2 severpe…   2015 Pedr…    21 Maj-… Wash…     1     1     1     1     1
#>  3 bandyje…   2015 Jett…    25 Maj-… Los …     2     2     2     1     1
#>  4 norrida…   2015 Dani…    22 Maj-… Detr…     1     3     2     1     1
#>  5 hesslke…   2015 Keit…    26 Maj-… Ariz…     1     1     1     0     1
#>  6 noelri02   2015 Rico…    26 Maj-… New …    13     1     1     5     1
#>  7 torreca…   2015 Carl…    32 Maj-… New …     1     1     1     1     1
#>  8 garcija…   2015 Jaso…    22 Maj-… Balt…     1     1     0     0     0
#>  9 gallayo…   2015 Yova…    29 Maj-… Texas     1     2     2     0     1
#> 10 panikjo…   2015 Joe …    24 Maj-… San …     4    14    12     5     5
#> # ℹ 754 more rows
#> # ℹ 21 more variables: X1B <dbl>, X2B <dbl>, X3B <dbl>, HR <dbl>,
#> #   RBI <dbl>, BB <dbl>, IBB <dbl>, uBB <dbl>, SO <dbl>, HBP <dbl>,
#> #   SH <dbl>, SF <dbl>, GDP <dbl>, SB <dbl>, CS <dbl>, BA <dbl>,
#> #   OBP <dbl>, SLG <dbl>, OPS <dbl>, wOBA <dbl>, wOBA_CON <dbl>
# }
```

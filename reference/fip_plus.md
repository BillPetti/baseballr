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

|                  |           |
|------------------|-----------|
| col_name         | types     |
| bbref_id         | character |
| season           | integer   |
| Name             | character |
| Age              | numeric   |
| Level            | character |
| Team             | character |
| G                | numeric   |
| GS               | numeric   |
| W                | numeric   |
| L                | numeric   |
| SV               | numeric   |
| IP               | numeric   |
| H                | numeric   |
| R                | numeric   |
| ER               | numeric   |
| uBB              | numeric   |
| BB               | numeric   |
| SO               | numeric   |
| HR               | numeric   |
| HBP              | numeric   |
| ERA              | numeric   |
| AB               | numeric   |
| X1B              | numeric   |
| X2B              | numeric   |
| X3B              | numeric   |
| IBB              | numeric   |
| GDP              | numeric   |
| SF               | numeric   |
| SB               | numeric   |
| CS               | numeric   |
| PO               | numeric   |
| BF               | numeric   |
| Pit              | numeric   |
| Str              | numeric   |
| StL              | numeric   |
| StS              | numeric   |
| GB.FB            | numeric   |
| LD               | numeric   |
| PU               | numeric   |
| WHIP             | numeric   |
| BAbip            | numeric   |
| SO9              | numeric   |
| SO.W             | numeric   |
| SO_perc          | numeric   |
| uBB_perc         | numeric   |
| SO_uBB           | numeric   |
| FIP              | numeric   |
| wOBA_against     | numeric   |
| wOBA_CON_against | numeric   |

## Examples

``` r
# \donttest{
  try({
    df <- bref_daily_pitcher("2015-04-05", "2015-04-30")
    fip_plus(df)
  })
#> ── MLB Daily Pitcher data from baseball-reference.com ──────────────────
#> ℹ Data updated: 2026-06-12 03:18:26 UTC
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

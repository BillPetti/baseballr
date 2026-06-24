# **Scrape FanGraphs.com Guts!**

Scrape historical FanGraphs Guts! table, wOBA, FIP coefficients and
constants

## Usage

``` r
fg_guts()
```

## Value

Returns a tibble of seasonal constants from FanGraphs

|            |         |                                                 |
|------------|---------|-------------------------------------------------|
| col_name   | types   | description                                     |
| season     | integer | Season (YYYY).                                  |
| lg_woba    | numeric | League-average wOBA for the season.             |
| woba_scale | numeric | wOBA scale factor (converts wOBA to runs).      |
| wBB        | numeric | Linear weight (runs) for an unintentional walk. |
| wHBP       | numeric | Linear weight (runs) for a hit-by-pitch.        |
| w1B        | numeric | Linear weight (runs) for a single.              |
| w2B        | numeric | Linear weight (runs) for a double.              |
| w3B        | numeric | Linear weight (runs) for a triple.              |
| wHR        | numeric | Linear weight (runs) for a home run.            |
| runSB      | numeric | Run value of a stolen base.                     |
| runCS      | numeric | Run value of a caught stealing.                 |
| lg_r_pa    | numeric | League runs per plate appearance.               |
| lg_r_w     | numeric | League runs per win.                            |
| cFIP       | numeric | FIP constant for the season.                    |

## Examples

``` r
# \donttest{
  try(fg_guts())
#> ── GUTS data from FanGraphs.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-24 02:05:24 UTC
#> # A tibble: 156 × 14
#>    season lg_woba woba_scale   wBB  wHBP   w1B   w2B   w3B   wHR runSB
#>     <int>   <dbl>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1   2026   0.317       1.24 0.7   0.731 0.892  1.26  1.60  2.05   0.2
#>  2   2025   0.313       1.23 0.691 0.722 0.882  1.25  1.58  2.04   0.2
#>  3   2024   0.31        1.24 0.689 0.72  0.882  1.25  1.59  2.05   0.2
#>  4   2023   0.318       1.20 0.696 0.726 0.883  1.24  1.57  2.00   0.2
#>  5   2022   0.31        1.26 0.689 0.72  0.884  1.26  1.60  2.07   0.2
#>  6   2021   0.314       1.21 0.692 0.722 0.879  1.24  1.57  2.01   0.2
#>  7   2020   0.32        1.18 0.699 0.728 0.883  1.24  1.56  1.98   0.2
#>  8   2019   0.32        1.16 0.69  0.719 0.87   1.22  1.53  1.94   0.2
#>  9   2018   0.315       1.23 0.69  0.72  0.88   1.25  1.58  2.03   0.2
#> 10   2017   0.321       1.18 0.693 0.723 0.877  1.23  1.55  1.98   0.2
#> # ℹ 146 more rows
#> # ℹ 4 more variables: runCS <dbl>, lg_r_pa <dbl>, lg_r_w <dbl>,
#> #   cFIP <dbl>
# }
```

# **Scrape Park Factors from FanGraphs**

This function allows you to scrape park factors for a given season from
FanGraphs.com.

This function allows you to scrape park factors by handedness from
FanGraphs.com for a given single year.

## Usage

``` r
fg_park(yr)

fg_park_hand(yr)
```

## Arguments

- yr:

  Season for which you want to scrape the park factors.

## Value

Returns a tibble of park factors.

|           |           |                                           |
|-----------|-----------|-------------------------------------------|
| col_name  | types     | description                               |
| season    | integer   | Season (YYYY).                            |
| home_team | character | Home team name.                           |
| basic_5yr | integer   | Basic 5-year park factor (100 = neutral). |
| 3yr       | integer   | 3-year park factor (100 = neutral).       |
| 1yr       | integer   | 1-year park factor (100 = neutral).       |
| single    | integer   | Park factor for singles.                  |
| double    | integer   | Park factor for doubles.                  |
| triple    | integer   | Park factor for triples.                  |
| hr        | integer   | Park factor for home runs.                |
| so        | integer   | Park factor for strikeouts.               |
| UIBB      | integer   | Park factor for unintentional walks.      |
| GB        | integer   | Park factor for ground balls.             |
| FB        | integer   | Park factor for fly balls.                |
| LD        | integer   | Park factor for line drives.              |
| IFFB      | integer   | Park factor for infield fly balls.        |
| FIP       | integer   | Park factor applied to FIP.               |

Returns a tibble of park factors by handedness.

|               |           |                                                |
|---------------|-----------|------------------------------------------------|
| col_name      | types     | description                                    |
| season        | integer   | Season (YYYY).                                 |
| home_team     | character | Home team name.                                |
| single_as_LHH | integer   | Singles park factor for left-handed hitters.   |
| single_as_RHH | integer   | Singles park factor for right-handed hitters.  |
| double_as_LHH | integer   | Doubles park factor for left-handed hitters.   |
| double_as_RHH | integer   | Doubles park factor for right-handed hitters.  |
| triple_as_LHH | integer   | Triples park factor for left-handed hitters.   |
| triple_as_RHH | integer   | Triples park factor for right-handed hitters.  |
| hr_as_LHH     | integer   | Home run park factor for left-handed hitters.  |
| hr_as_RHH     | integer   | Home run park factor for right-handed hitters. |

## Examples

``` r
# \donttest{
  try(fg_park(2013))
#> ── Park Factors data from FanGraphs.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:45:05 UTC
#> # A tibble: 30 × 16
#>    season home_team basic_5yr `3yr` `1yr` single double triple    hr
#>     <int> <chr>         <int> <int> <int>  <int>  <int>  <int> <int>
#>  1   2013 Angels           95    95    98    100     96     91    93
#>  2   2013 Orioles         103   102   103    101     99     86   108
#>  3   2013 Red Sox         105   104    98    103    114    105    96
#>  4   2013 White Sox       102   105   100     99     97     91   108
#>  5   2013 Indians         100    97    97    100    103     81   101
#>  6   2013 Tigers          101   103   106    101     98    126    98
#>  7   2013 Royals          101   102   104    101    103    114    93
#>  8   2013 Twins           101   103   101    102    101    106    98
#>  9   2013 Yankees         101   100   104    100     97     84   110
#> 10   2013 Athletics        97    97    95     99    100    105    93
#> # ℹ 20 more rows
#> # ℹ 7 more variables: so <int>, UIBB <int>, GB <int>, FB <int>,
#> #   LD <int>, IFFB <int>, FIP <int>
# }
# \donttest{
  try(fg_park_hand(2013))
#> ── Park Factors by Handedness data from FanGraphs.com ──────────────────
#> ℹ Data updated: 2026-06-08 03:45:06 UTC
#> # A tibble: 30 × 10
#>    season home_team single_as_LHH single_as_RHH double_as_LHH
#>     <int> <chr>             <int>         <int>         <int>
#>  1   2013 Angels               98           100            95
#>  2   2013 Orioles             102           102           101
#>  3   2013 Red Sox             104           103           121
#>  4   2013 White Sox            97           101            99
#>  5   2013 Indians             102            99           104
#>  6   2013 Tigers              102           101            93
#>  7   2013 Royals              100           101           104
#>  8   2013 Twins               101           103            97
#>  9   2013 Yankees             100            99            98
#> 10   2013 Athletics            99           100            99
#> # ℹ 20 more rows
#> # ℹ 5 more variables: double_as_RHH <int>, triple_as_LHH <int>,
#> #   triple_as_RHH <int>, hr_as_LHH <int>, hr_as_RHH <int>
# }
```

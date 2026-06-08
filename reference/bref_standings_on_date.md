# **Scrape MLB Standings on a Given Date**

This function allows you to scrape the standings from MLB for any date
you choose.

## Usage

``` r
bref_standings_on_date(date, division, from = FALSE)
```

## Arguments

- date:

  a date object

- division:

  One or more of AL East, AL Central, AL West, AL Overall, NL East, NL
  Central, NL West, and NL Overall

- from:

  a logical indicating whether you want standings up to and including
  the date (FALSE, default) or rather standings for games played after
  the date

## Value

Returns a tibble of MLB standings, one row per team in the requested
division, with the following columns:

|          |           |                                                       |
|----------|-----------|-------------------------------------------------------|
| col_name | types     | description                                           |
| Tm       | character | Team abbreviation.                                    |
| W        | integer   | Wins as of the requested date.                        |
| L        | integer   | Losses as of the requested date.                      |
| W-L%     | numeric   | Winning percentage.                                   |
| GB       | character | Games behind the division leader (– for the leader).  |
| RS       | integer   | Runs scored.                                          |
| RA       | integer   | Runs allowed.                                         |
| pythW-L% | numeric   | Pythagorean (expected) winning percentage from RS/RA. |

## Examples

``` r
# \donttest{
  try(bref_standings_on_date(date = "2015-08-04", division = "AL East"))
#> ── MLB Standings on Date data from baseball-reference.com ──────────────
#> ℹ Data updated: 2026-06-08 04:37:41 UTC
#> # A tibble: 5 × 8
#>   Tm        W     L `W-L%` GB       RS    RA `pythW-L%`
#>   <chr> <int> <int>  <dbl> <chr> <int> <int>      <dbl>
#> 1 NYY      60    45  0.571 --      525   455      0.565
#> 2 TOR      56    52  0.519 5.5     569   457      0.599
#> 3 BAL      54    52  0.509 6.5     464   415      0.551
#> 4 TBR      54    54  0.5   7.5     399   408      0.49 
#> 5 BOS      47    60  0.439 14.0    449   524      0.43 
# }
```

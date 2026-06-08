# **Acquire win probability for Major and Minor League games**

**Acquire win probability for Major and Minor League games**

## Usage

``` r
mlb_game_wp(game_pk, timecode = NULL)
```

## Arguments

- game_pk:

  The game_pk for the game requested

- timecode:

  The time code for the MLB game (format: MMDDYYYY_HHMMSS)

## Value

Returns a tibble that includes time codes from the game_pk requested

|  |  |  |
|----|----|----|
| col_name | types | description |
| home_team_win_probability | numeric | Home team win probability (percent) entering the at-bat. |
| away_team_win_probability | numeric | Away team win probability (percent) entering the at-bat. |
| home_team_win_probability_added | numeric | Change in home team win probability attributed to the at-bat. |
| at_bat_index | integer | Zero-based index of the at-bat within the game. |
| leverage_index | numeric | Leverage index quantifying the importance of the at-bat situation. |

## Examples

``` r
# \donttest{
  try(mlb_game_wp(game_pk = 531060))
#> ── MLB Game Win Probability data from MLB.com ─────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:20:15 UTC
#> # A tibble: 74 × 5
#>    home_team_win_probabi…¹ away_team_win_probab…² home_team_win_probab…³
#>                      <dbl>                  <dbl>                  <dbl>
#>  1                    52.2                   47.8                    2.2
#>  2                    53.8                   46.2                    1.6
#>  3                    52.6                   47.4                   -1.2
#>  4                    54.8                   45.2                    2.2
#>  5                    52.6                   47.4                   -2.2
#>  6                    51                     49                     -1.6
#>  7                    50                     50                     -1  
#>  8                    46.2                   53.8                   -3.8
#>  9                    49.7                   50.3                    3.5
#> 10                    52.8                   47.2                    3.1
#> # ℹ 64 more rows
#> # ℹ abbreviated names: ¹​home_team_win_probability,
#> #   ²​away_team_win_probability, ³​home_team_win_probability_added
#> # ℹ 2 more variables: at_bat_index <int>, leverage_index <dbl>
# }
```

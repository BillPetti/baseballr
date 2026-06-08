# **Acquire time codes for Major and Minor League games**

**Acquire time codes for Major and Minor League games**

## Usage

``` r
mlb_game_timecodes(game_pk)
```

## Arguments

- game_pk:

  The game_pk for the game requested

## Value

Returns a tibble that includes time codes from the game_pk requested

|           |           |                                                      |
|-----------|-----------|------------------------------------------------------|
| col_name  | types     | description                                          |
| timecodes | character | Play snapshot time code in 'YYYYMMDD_HHMMSS' format. |

## Examples

``` r
# \donttest{
  try(mlb_game_timecodes(game_pk = 632970))
#> ── MLB Game Timecodes data from MLB.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:20:14 UTC
#> # A tibble: 473 × 1
#>    timecodes      
#>    <chr>          
#>  1 20210808_213535
#>  2 20210808_224317
#>  3 20210808_230851
#>  4 20210808_230925
#>  5 20210808_230927
#>  6 20210808_230954
#>  7 20210808_231025
#>  8 20210808_231038
#>  9 20210808_231047
#> 10 20210808_231115
#> # ℹ 463 more rows
# }
```

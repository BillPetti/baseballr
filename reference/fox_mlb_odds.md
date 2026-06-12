# **Get Fox Sports MLB game odds**

**Get Fox Sports MLB game odds**

## Usage

``` r
fox_mlb_odds(game_id)
```

## Arguments

- game_id:

  Fox Bifrost event id.

## Value

A `baseballr_data` tibble, one row per team (`game_id`, `team`, plus
six-pack odds columns: run line / to-win / total). Empty when no market.

## Examples

``` r
 try(fox_mlb_odds("95687")) 
#> ── Fox Sports MLB odds ────────────────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 14:09:05 UTC
#> # A tibble: 2 × 5
#>   game_id team              run_line to_win total
#>   <chr>   <chr>             <chr>    <chr>  <chr>
#> 1 95687   Seattle Mariners  -1.5     -122   O 8.5
#> 2 95687   Baltimore Orioles +1.5     +102   U 8.5
```

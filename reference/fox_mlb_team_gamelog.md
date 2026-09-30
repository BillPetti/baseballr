# **Get Fox Sports MLB team game log**

**Get Fox Sports MLB team game log**

## Usage

``` r
fox_mlb_team_gamelog(team_id)
```

## Arguments

- team_id:

  Fox Bifrost team id.

## Value

A `baseballr_data` tibble (long): `team_id`, `season_type`, `category`,
`game_id`, `game_date`, `opponent`, `stat`, `value`.

## Examples

``` r
 try(fox_mlb_team_gamelog("1")) 
#> ── Fox Sports MLB gamelog ─────────────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-09-30 14:28:05 UTC
#> # A tibble: 95 × 8
#>    team_id season_type   category game_id game_date opponent stat  value
#>    <chr>   <chr>         <chr>    <chr>   <chr>     <chr>    <chr> <chr>
#>  1 1       REGULAR SEAS… hitting  97044   9/25      @NYY     ab    38   
#>  2 1       REGULAR SEAS… hitting  97044   9/25      @NYY     h     11   
#>  3 1       REGULAR SEAS… hitting  97044   9/25      @NYY     r     3    
#>  4 1       REGULAR SEAS… hitting  97044   9/25      @NYY     x2b   2    
#>  5 1       REGULAR SEAS… hitting  97044   9/25      @NYY     x3b   0    
#>  6 1       REGULAR SEAS… hitting  97044   9/25      @NYY     hr    1    
#>  7 1       REGULAR SEAS… hitting  97044   9/25      @NYY     rbi   3    
#>  8 1       REGULAR SEAS… hitting  97044   9/25      @NYY     bb    4    
#>  9 1       REGULAR SEAS… hitting  97044   9/25      @NYY     so    17   
#> 10 1       REGULAR SEAS… hitting  97044   9/25      @NYY     sb    2    
#> # ℹ 85 more rows
```

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
#> ℹ Data updated: 2026-07-23 17:33:40 UTC
#> # A tibble: 95 × 8
#>    team_id season_type   category game_id game_date opponent stat  value
#>    <chr>   <chr>         <chr>    <chr>   <chr>     <chr>    <chr> <chr>
#>  1 1       REGULAR SEAS… hitting  96201   7/22      @BOS     ab    34   
#>  2 1       REGULAR SEAS… hitting  96201   7/22      @BOS     h     9    
#>  3 1       REGULAR SEAS… hitting  96201   7/22      @BOS     r     5    
#>  4 1       REGULAR SEAS… hitting  96201   7/22      @BOS     x2b   2    
#>  5 1       REGULAR SEAS… hitting  96201   7/22      @BOS     x3b   1    
#>  6 1       REGULAR SEAS… hitting  96201   7/22      @BOS     hr    0    
#>  7 1       REGULAR SEAS… hitting  96201   7/22      @BOS     rbi   3    
#>  8 1       REGULAR SEAS… hitting  96201   7/22      @BOS     bb    4    
#>  9 1       REGULAR SEAS… hitting  96201   7/22      @BOS     so    8    
#> 10 1       REGULAR SEAS… hitting  96201   7/22      @BOS     sb    1    
#> # ℹ 85 more rows
```

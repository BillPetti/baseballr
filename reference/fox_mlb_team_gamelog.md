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
#> ℹ Data updated: 2026-06-24 02:05:39 UTC
#> # A tibble: 95 × 8
#>    team_id season_type   category game_id game_date opponent stat  value
#>    <chr>   <chr>         <chr>    <chr>   <chr>     <chr>    <chr> <chr>
#>  1 1       REGULAR SEAS… hitting  95861   6/22      @LAA     ab    37   
#>  2 1       REGULAR SEAS… hitting  95861   6/22      @LAA     h     11   
#>  3 1       REGULAR SEAS… hitting  95861   6/22      @LAA     r     6    
#>  4 1       REGULAR SEAS… hitting  95861   6/22      @LAA     x2b   1    
#>  5 1       REGULAR SEAS… hitting  95861   6/22      @LAA     x3b   1    
#>  6 1       REGULAR SEAS… hitting  95861   6/22      @LAA     hr    2    
#>  7 1       REGULAR SEAS… hitting  95861   6/22      @LAA     rbi   6    
#>  8 1       REGULAR SEAS… hitting  95861   6/22      @LAA     bb    1    
#>  9 1       REGULAR SEAS… hitting  95861   6/22      @LAA     so    12   
#> 10 1       REGULAR SEAS… hitting  95861   6/22      @LAA     sb    1    
#> # ℹ 85 more rows
```

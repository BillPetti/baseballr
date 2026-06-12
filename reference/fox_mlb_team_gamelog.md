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
#> ℹ Data updated: 2026-06-12 11:23:16 UTC
#> # A tibble: 95 × 8
#>    team_id season_type   category game_id game_date opponent stat  value
#>    <chr>   <chr>         <chr>    <chr>   <chr>     <chr>    <chr> <chr>
#>  1 1       REGULAR SEAS… hitting  95717   6/11      SEA      ab    30   
#>  2 1       REGULAR SEAS… hitting  95717   6/11      SEA      h     7    
#>  3 1       REGULAR SEAS… hitting  95717   6/11      SEA      r     7    
#>  4 1       REGULAR SEAS… hitting  95717   6/11      SEA      x2b   1    
#>  5 1       REGULAR SEAS… hitting  95717   6/11      SEA      x3b   0    
#>  6 1       REGULAR SEAS… hitting  95717   6/11      SEA      hr    2    
#>  7 1       REGULAR SEAS… hitting  95717   6/11      SEA      rbi   6    
#>  8 1       REGULAR SEAS… hitting  95717   6/11      SEA      bb    3    
#>  9 1       REGULAR SEAS… hitting  95717   6/11      SEA      so    7    
#> 10 1       REGULAR SEAS… hitting  95717   6/11      SEA      sb    0    
#> # ℹ 85 more rows
```

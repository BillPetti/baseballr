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
#> ℹ Data updated: 2026-08-26 19:18:25 UTC
#> # A tibble: 95 × 8
#>    team_id season_type   category game_id game_date opponent stat  value
#>    <chr>   <chr>         <chr>    <chr>   <chr>     <chr>    <chr> <chr>
#>  1 1       REGULAR SEAS… hitting  96642   8/25      @STL     ab    43   
#>  2 1       REGULAR SEAS… hitting  96642   8/25      @STL     h     16   
#>  3 1       REGULAR SEAS… hitting  96642   8/25      @STL     r     13   
#>  4 1       REGULAR SEAS… hitting  96642   8/25      @STL     x2b   4    
#>  5 1       REGULAR SEAS… hitting  96642   8/25      @STL     x3b   0    
#>  6 1       REGULAR SEAS… hitting  96642   8/25      @STL     hr    2    
#>  7 1       REGULAR SEAS… hitting  96642   8/25      @STL     rbi   13   
#>  8 1       REGULAR SEAS… hitting  96642   8/25      @STL     bb    2    
#>  9 1       REGULAR SEAS… hitting  96642   8/25      @STL     so    13   
#> 10 1       REGULAR SEAS… hitting  96642   8/25      @STL     sb    0    
#> # ℹ 85 more rows
```

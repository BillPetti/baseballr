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
#> ℹ Data updated: 2026-08-27 10:42:03 UTC
#> # A tibble: 95 × 8
#>    team_id season_type   category game_id game_date opponent stat  value
#>    <chr>   <chr>         <chr>    <chr>   <chr>     <chr>    <chr> <chr>
#>  1 1       REGULAR SEAS… hitting  96661   8/26      @STL     ab    38   
#>  2 1       REGULAR SEAS… hitting  96661   8/26      @STL     h     13   
#>  3 1       REGULAR SEAS… hitting  96661   8/26      @STL     r     8    
#>  4 1       REGULAR SEAS… hitting  96661   8/26      @STL     x2b   3    
#>  5 1       REGULAR SEAS… hitting  96661   8/26      @STL     x3b   0    
#>  6 1       REGULAR SEAS… hitting  96661   8/26      @STL     hr    2    
#>  7 1       REGULAR SEAS… hitting  96661   8/26      @STL     rbi   8    
#>  8 1       REGULAR SEAS… hitting  96661   8/26      @STL     bb    4    
#>  9 1       REGULAR SEAS… hitting  96661   8/26      @STL     so    7    
#> 10 1       REGULAR SEAS… hitting  96661   8/26      @STL     sb    0    
#> # ℹ 85 more rows
```

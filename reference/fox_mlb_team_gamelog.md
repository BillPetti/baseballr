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
#> ℹ Data updated: 2026-09-10 00:15:40 UTC
#> # A tibble: 95 × 8
#>    team_id season_type   category game_id game_date opponent stat  value
#>    <chr>   <chr>         <chr>    <chr>   <chr>     <chr>    <chr> <chr>
#>  1 1       REGULAR SEAS… hitting  96818   9/8       CLE      ab    33   
#>  2 1       REGULAR SEAS… hitting  96818   9/8       CLE      h     6    
#>  3 1       REGULAR SEAS… hitting  96818   9/8       CLE      r     5    
#>  4 1       REGULAR SEAS… hitting  96818   9/8       CLE      x2b   3    
#>  5 1       REGULAR SEAS… hitting  96818   9/8       CLE      x3b   0    
#>  6 1       REGULAR SEAS… hitting  96818   9/8       CLE      hr    1    
#>  7 1       REGULAR SEAS… hitting  96818   9/8       CLE      rbi   5    
#>  8 1       REGULAR SEAS… hitting  96818   9/8       CLE      bb    3    
#>  9 1       REGULAR SEAS… hitting  96818   9/8       CLE      so    8    
#> 10 1       REGULAR SEAS… hitting  96818   9/8       CLE      sb    0    
#> # ℹ 85 more rows
```

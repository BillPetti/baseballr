# **Get Fox Sports MLB team stat leaders**

**Get Fox Sports MLB team stat leaders**

## Usage

``` r
fox_mlb_team_stats(team_id)
```

## Arguments

- team_id:

  Fox Bifrost team id.

## Value

A `baseballr_data` tibble: `team_id`, `category`, `stat`,
`stat_abbreviation`, `player`, `value`.

## Examples

``` r
 try(fox_mlb_team_stats("1")) 
#> ── Fox Sports MLB team_stats ──────────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-07-23 17:33:41 UTC
#> # A tibble: 28 × 6
#>    team_id category     stat              stat_abbreviation player value
#>    <chr>   <chr>        <chr>             <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Batting Average   AVG               Taylo… .251 
#>  2 1       PLAYER STATS Home Runs         HR                Pete … 21   
#>  3 1       PLAYER STATS Runs Batted In    RBI               Pete … 66   
#>  4 1       PLAYER STATS Stolen Bases      SB                Leody… 10   
#>  5 1       PLAYER STATS Runs Created      RC                Pete … 63.08
#>  6 1       PLAYER STATS Wins              W                 Brand… 8    
#>  7 1       PLAYER STATS Earned Run Avera… ERA               Kyle … 3.49 
#>  8 1       PLAYER STATS Strikeouts        SO                Kyle … 110  
#>  9 1       PLAYER STATS Pitches / Inning  PC/IP             Shane… 16.4 
#> 10 1       PLAYER STATS Strikeouts/9 Inn… SO/9              Kyle … 8.7  
#> # ℹ 18 more rows
```

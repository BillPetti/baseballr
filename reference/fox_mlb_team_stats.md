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
#> ℹ Data updated: 2026-08-26 19:18:26 UTC
#> # A tibble: 28 × 6
#>    team_id category     stat              stat_abbreviation player value
#>    <chr>   <chr>        <chr>             <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Batting Average   AVG               Pete … .276 
#>  2 1       PLAYER STATS Home Runs         HR                Pete … 31   
#>  3 1       PLAYER STATS Runs Batted In    RBI               Pete … 89   
#>  4 1       PLAYER STATS Stolen Bases      SB                Leody… 10   
#>  5 1       PLAYER STATS Runs Created      RC                Pete … 94.38
#>  6 1       PLAYER STATS Isolated Power    ISO               Pete … .236 
#>  7 1       PLAYER STATS Wins              W                 Trevo… 9    
#>  8 1       PLAYER STATS Earned Run Avera… ERA               Kyle … 3.70 
#>  9 1       PLAYER STATS Strikeouts        SO                Shane… 135  
#> 10 1       PLAYER STATS Pitches / Inning  PC/IP             Trevo… 16.3 
#> # ℹ 18 more rows
```

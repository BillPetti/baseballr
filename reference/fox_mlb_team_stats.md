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
#> ℹ Data updated: 2026-06-12 12:24:19 UTC
#> # A tibble: 29 × 6
#>    team_id category     stat              stat_abbreviation player value
#>    <chr>   <chr>        <chr>             <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Batting Average   AVG               Taylo… .263 
#>  2 1       PLAYER STATS Home Runs         HR                Pete … 15   
#>  3 1       PLAYER STATS Runs Batted In    RBI               Pete … 44   
#>  4 1       PLAYER STATS Stolen Bases      SB                Leody… 8    
#>  5 1       PLAYER STATS Runs Created      RC                Taylo… 43.16
#>  6 1       PLAYER STATS Isolated Power    ISO               Pete … .212 
#>  7 1       PLAYER STATS Wins              W                 Brand… 5    
#>  8 1       PLAYER STATS Earned Run Avera… ERA               Shane… 4.09 
#>  9 1       PLAYER STATS Strikeouts        SO                Kyle … 73   
#> 10 1       PLAYER STATS Pitches / Inning  PC/IP             Shane… 16.1 
#> # ℹ 19 more rows
```

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
#> ℹ Data updated: 2026-09-10 00:48:13 UTC
#> # A tibble: 28 × 6
#>    team_id category     stat              stat_abbreviation player value
#>    <chr>   <chr>        <chr>             <chr>             <chr>  <chr>
#>  1 1       PLAYER STATS Batting Average   AVG               Pete … .268 
#>  2 1       PLAYER STATS Home Runs         HR                Pete … 34   
#>  3 1       PLAYER STATS Runs Batted In    RBI               Pete … 97   
#>  4 1       PLAYER STATS Stolen Bases      SB                Leody… 11   
#>  5 1       PLAYER STATS Runs Created      RC                Pete … 100.…
#>  6 1       PLAYER STATS Isolated Power    ISO               Pete … .231 
#>  7 1       PLAYER STATS Wins              W                 Trevo… 10   
#>  8 1       PLAYER STATS Earned Run Avera… ERA               Kyle … 3.91 
#>  9 1       PLAYER STATS Strikeouts        SO                Shane… 145  
#> 10 1       PLAYER STATS Pitches / Inning  PC/IP             Trevo… 16.1 
#> # ℹ 18 more rows
```

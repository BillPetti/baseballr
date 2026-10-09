# **Get Fox Sports MLB team roster**

**Get Fox Sports MLB team roster**

## Usage

``` r
fox_mlb_team_roster(team_id)
```

## Arguments

- team_id:

  Fox Bifrost team id (e.g. `"1"`). Discover via the league team
  directory.

## Value

A `baseballr_data` tibble, one row per player: `team_id`,
`position_group`, `player`, position/age/etc. columns, `athlete_id`.

## Examples

``` r
 try(fox_mlb_team_roster("1")) 
#> ── Fox Sports MLB roster ──────────────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-10-09 04:40:41 UTC
#> # A tibble: 48 × 9
#>    team_id position_group player         pos   age   ht     wt    school
#>    <chr>   <chr>          <chr>          <chr> <chr> <chr>  <chr> <chr> 
#>  1 1       PITCHER        Keegan Akin    P     31    "6'0\… 235 … Weste…
#>  2 1       PITCHER        Chris Bassitt  P     37    "6'5\… 220 … Akron 
#>  3 1       PITCHER        Félix Bautista P     31    "6'8\… 285 … -     
#>  4 1       PITCHER        Shane Baz      P     27    "6'3\… 200 … Conco…
#>  5 1       PITCHER        Kyle Bradish   P     30    "6'3\… 215 … New M…
#>  6 1       PITCHER        Yennier Cano   P     32    "6'4\… 245 … -     
#>  7 1       PITCHER        Luis De León   P     23    "6'3\… 168 … -     
#>  8 1       PITCHER        Zach Eflin     P     32    "6'6\… 230 … Paul …
#>  9 1       PITCHER        Cameron Foster P     27    "6'5\… 240 … McNee…
#> 10 1       PITCHER        Rico Garcia    P     32    "5'9\… 215 … Hawai…
#> # ℹ 38 more rows
#> # ℹ 1 more variable: athlete_id <chr>
```

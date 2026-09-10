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
#> ℹ Data updated: 2026-09-10 00:15:40 UTC
#> # A tibble: 28 × 9
#>    team_id position_group player          pos   age   ht    wt    school
#>    <chr>   <chr>          <chr>           <chr> <chr> <chr> <chr> <chr> 
#>  1 1       PITCHER        Chris Bassitt   P     37    "6'5… 220 … Akron 
#>  2 1       PITCHER        Shane Baz       P     27    "6'3… 200 … Conco…
#>  3 1       PITCHER        Kyle Bradish    P     29    "6'3… 215 … New M…
#>  4 1       PITCHER        Yennier Cano    P     32    "6'4… 245 … -     
#>  5 1       PITCHER        Luis De León    P     23    "6'3… 168 … -     
#>  6 1       PITCHER        Rico Garcia     P     32    "5'9… 215 … Hawai…
#>  7 1       PITCHER        Alex Hoppe      P     27    "6'1… 200 … UNC G…
#>  8 1       PITCHER        Andrew Kittred… P     36    "6'1… 235 … Washi…
#>  9 1       PITCHER        Anthony Nunez   P     25    "6'2… 220 … Tampa 
#> 10 1       PITCHER        Trevor Rogers   P     28    "6'5… 230 … Carls…
#> # ℹ 18 more rows
#> # ℹ 1 more variable: athlete_id <chr>
```

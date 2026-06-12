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
#> ℹ Data updated: 2026-06-12 11:56:24 UTC
#> # A tibble: 26 × 9
#>    team_id position_group player          pos   age   ht    wt    school
#>    <chr>   <chr>          <chr>           <chr> <chr> <chr> <chr> <chr> 
#>  1 1       PITCHER        Keegan Akin     P     31    "6'0… 235 … Weste…
#>  2 1       PITCHER        Shane Baz       P     26    "6'3… 200 … Conco…
#>  3 1       PITCHER        Kyle Bradish    P     29    "6'3… 215 … New M…
#>  4 1       PITCHER        Yennier Cano    P     32    "6'4… 245 … -     
#>  5 1       PITCHER        Rico Garcia     P     32    "5'9… 215 … Hawai…
#>  6 1       PITCHER        Trey Gibson     P     24    "6'5… 240 … Liber…
#>  7 1       PITCHER        Andrew Kittred… P     36    "6'1… 235 … Washi…
#>  8 1       PITCHER        Anthony Nunez   P     24    "6'2… 220 … Tampa 
#>  9 1       PITCHER        Trevor Rogers   P     28    "6'5… 230 … Carls…
#> 10 1       PITCHER        Albert Suárez   P     36    "6'3… 235 … -     
#> # ℹ 16 more rows
#> # ℹ 1 more variable: athlete_id <chr>
```

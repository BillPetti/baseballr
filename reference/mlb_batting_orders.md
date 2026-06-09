# **Retrieve batting orders for a given MLB game**

**Retrieve batting orders for a given MLB game**

## Usage

``` r
mlb_batting_orders(game_pk, type = "starting")
```

## Arguments

- game_pk:

  The unique game_pk identifier for the game

- type:

  Whether to just return the starting lineup ('starting') or all batters
  that appeared ('all')

## Value

Returns a tibble that includes probable starting pitchers and the home
plate umpire for the `game_pk` requested

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | MLB player ID. |
| fullName | character | Player full name. |
| abbreviation | character | Fielding position abbreviation. |
| batting_order | character | Spot in the batting order (1-9). |
| batting_position_num | character | Sub-position within the lineup spot (0 = starter). |
| team | character | Side of the matchup ('home' or 'away'). |
| teamName | character | Team name. |
| teamID | integer | MLB team ID. |

## Examples

``` r
# \donttest{
  try(mlb_batting_orders(game_pk=566001))
#> ── MLB Game Starting Batting Order data from MLB.com ───────────────────
#> ℹ Data updated: 2026-06-09 20:43:27 UTC
#> # A tibble: 18 × 8
#>        id fullName abbreviation batting_order batting_position_num team 
#>     <int> <chr>    <chr>        <chr>         <chr>                <chr>
#>  1 606299 José Pe… 2B           1             0                    away 
#>  2 458015 Joey Vo… 1B           2             0                    away 
#>  3 553993 Eugenio… 3B           3             0                    away 
#>  4 608385 Jesse W… LF           4             0                    away 
#>  5 624577 Yasiel … RF           5             0                    away 
#>  6 594988 Scott S… CF           6             0                    away 
#>  7 578428 Jose Ig… SS           7             0                    away 
#>  8 571466 Tucker … C            8             0                    away 
#>  9 543699 Tanner … P            9             0                    away 
#> 10 643446 Jeff Mc… 2B           1             0                    home 
#> 11 624413 Pete Al… 1B           2             0                    home 
#> 12 607043 Brandon… LF           3             0                    home 
#> 13 624424 Michael… RF           4             0                    home 
#> 14 453943 Todd Fr… 3B           5             0                    home 
#> 15 467092 Wilson … C            6             0                    home 
#> 16 642708 Amed Ro… SS           7             0                    home 
#> 17 501571 Juan La… CF           8             0                    home 
#> 18 554430 Zack Wh… P            9             0                    home 
#> # ℹ 2 more variables: teamName <chr>, teamID <int>
# }
```

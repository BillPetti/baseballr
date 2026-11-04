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
#> ℹ Data updated: 2026-11-04 06:10:34 UTC
#> # A tibble: 18 × 9
#>        id fullName         nameSlug           abbreviation batting_order
#>     <int> <chr>            <chr>              <chr>        <chr>        
#>  1 606299 José Peraza      jose-peraza-606299 2B           1            
#>  2 458015 Joey Votto       joey-votto-458015  1B           2            
#>  3 553993 Eugenio Suárez   eugenio-suarez-55… 3B           3            
#>  4 608385 Jesse Winker     jesse-winker-6083… LF           4            
#>  5 624577 Yasiel Puig      yasiel-puig-624577 RF           5            
#>  6 594988 Scott Schebler   scott-schebler-59… CF           6            
#>  7 578428 Jose Iglesias    jose-iglesias-578… SS           7            
#>  8 571466 Tucker Barnhart  tucker-barnhart-5… C            8            
#>  9 543699 Tanner Roark     tanner-roark-5436… P            9            
#> 10 643446 Jeff McNeil      jeff-mcneil-643446 2B           1            
#> 11 624413 Pete Alonso      pete-alonso-624413 1B           2            
#> 12 607043 Brandon Nimmo    brandon-nimmo-607… LF           3            
#> 13 624424 Michael Conforto michael-conforto-… RF           4            
#> 14 453943 Todd Frazier     todd-frazier-4539… 3B           5            
#> 15 467092 Wilson Ramos     wilson-ramos-4670… C            6            
#> 16 642708 Amed Rosario     amed-rosario-6427… SS           7            
#> 17 501571 Juan Lagares     juan-lagares-5015… CF           8            
#> 18 554430 Zack Wheeler     zack-wheeler-5544… P            9            
#> # ℹ 4 more variables: batting_position_num <chr>, team <chr>,
#> #   teamName <chr>, teamID <int>
# }
```

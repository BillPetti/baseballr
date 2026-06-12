# **Find MLB All-Star Write-ins**

**Find MLB All-Star Write-ins**

## Usage

``` r
mlb_all_star_write_ins(league_id = NULL, season = NULL)
```

## Arguments

- league_id:

  League ID for league all-star ballot of interest.

- season:

  The season of the all-star ballot.

## Value

Returns a tibble with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | integer | MLB player ID. |
| full_name | character | Player full name. |
| link | character | MLB Stats API relative resource link. |
| first_name | character | Player first name. |
| last_name | character | Player last name. |
| primary_number | character | Player uniform number. |
| birth_date | character | Birth date (YYYY-MM-DD). |
| current_age | integer | Current age in years. |
| birth_city | character | City of birth. |
| birth_state_province | character | State or province of birth. |
| birth_country | character | Country of birth. |
| height | character | Height (feet and inches). |
| weight | integer | Weight in pounds. |
| active | logical | Whether the player is currently active. |
| use_name | character | Preferred first name. |
| use_last_name | character | Preferred last name. |
| middle_name | character | Player middle name. |
| boxscore_name | character | Name as shown in box scores. |
| nick_name | character | Player nickname. |
| gender | character | Player gender. |
| is_player | logical | Whether the person is a player. |
| is_verified | logical | Whether the player profile is verified. |
| draft_year | integer | Year the player was drafted. |
| mlb_debut_date | character | MLB debut date (YYYY-MM-DD). |
| name_first_last | character | Name in first-last order. |
| name_slug | character | URL-friendly name slug. |
| first_last_name | character | First and last name. |
| last_first_name | character | Name in last, first order. |
| last_init_name | character | Last name with first initial. |
| init_last_name | character | First initial with last name. |
| full_fml_name | character | Full name (first-middle-last). |
| full_lfm_name | character | Full name (last-first-middle). |
| strike_zone_top | numeric | Top of the player's strike zone (feet). |
| strike_zone_bottom | numeric | Bottom of the player's strike zone (feet). |
| pronunciation | character | Phonetic name pronunciation. |
| last_played_date | character | Date of last MLB game played. |
| name_title | character | Name title. |
| name_suffix | character | Name suffix (e.g. Jr., III). |
| name_matrilineal | character | Maternal family name. |
| bat_side_code | character | Batting side code (L/R/S). |
| bat_side_description | character | Batting side description. |
| pitch_hand_code | character | Throwing hand code (L/R). |
| pitch_hand_description | character | Throwing hand description. |
| league_id | numeric | MLB league ID. |
| season | numeric | Season (YYYY). |

## Examples

``` r
# \donttest{
 try(mlb_all_star_write_ins(league_id = 103, season = 2021))
#> ── MLB All-Star Write-Ins data from MLB.com ───────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 13:46:11 UTC
#> # A tibble: 490 × 45
#>    player_id full_name      link     first_name last_name primary_number
#>        <int> <chr>          <chr>    <chr>      <chr>     <chr>         
#>  1    595978 Austin Hedges  /api/v1… Austin     Hedges    27            
#>  2    605194 Jharel Cotton  /api/v1… Jharel     Cotton    NA            
#>  3    451584 Wade Davis     /api/v1… Wade       Davis     40            
#>  4    663554 Casey Mize     /api/v1… Casey      Mize      12            
#>  5    609280 Miguel Andujar /api/v1… Miguel     Andujar   41            
#>  6    521230 Liam Hendriks  /api/v1… Liam       Hendriks  31            
#>  7    642048 Tayler Saucedo /api/v1… Tayler     Saucedo   55            
#>  8    451594 Dexter Fowler  /api/v1… William    Fowler    25            
#>  9    667674 Jack Kruger    /api/v1… Timothy    Kruger    NA            
#> 10    667670 Brent Rooker   /api/v1… Terry      Rooker    25            
#> # ℹ 480 more rows
#> # ℹ 39 more variables: birth_date <chr>, current_age <int>,
#> #   birth_city <chr>, birth_state_province <chr>, birth_country <chr>,
#> #   height <chr>, weight <int>, active <lgl>, use_name <chr>,
#> #   use_last_name <chr>, middle_name <chr>, boxscore_name <chr>,
#> #   nick_name <chr>, gender <chr>, is_player <lgl>, is_verified <lgl>,
#> #   draft_year <int>, mlb_debut_date <chr>, name_first_last <chr>, …
# }
```

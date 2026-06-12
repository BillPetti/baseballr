# **Find MLB All-Star Ballots**

**Find MLB All-Star Ballots**

## Usage

``` r
mlb_all_star_ballots(league_id = NULL, season = NULL)
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
| name_matrilineal | character | Maternal family name. |
| is_player | logical | Whether the person is a player. |
| is_verified | logical | Whether the player profile is verified. |
| pronunciation | character | Phonetic name pronunciation. |
| last_played_date | character | Date of last MLB game played. |
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
| birth_state_province | character | State or province of birth. |
| draft_year | integer | Year the player was drafted. |
| name_title | character | Name title. |
| name_suffix | character | Name suffix (e.g. Jr., III). |
| primary_position_code | character | Primary fielding position code. |
| primary_position_name | character | Primary fielding position name. |
| primary_position_type | character | Primary position type (e.g. Infielder). |
| primary_position_abbreviation | character | Primary position abbreviation. |
| bat_side_code | character | Batting side code (L/R/S). |
| bat_side_description | character | Batting side description. |
| pitch_hand_code | character | Throwing hand code (L/R). |
| pitch_hand_description | character | Throwing hand description. |
| league_id | numeric | MLB league ID. |
| season | numeric | Season (YYYY). |

## Examples

``` r
# \donttest{
 try(mlb_all_star_ballots(league_id = 103, season = 2021))
#> ── MLB All-Star Ballots data from MLB.com ─────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:24:35 UTC
#> # A tibble: 136 × 49
#>    player_id full_name         link  first_name last_name primary_number
#>        <int> <chr>             <chr> <chr>      <chr>     <chr>         
#>  1    547989 José Abreu        /api… José       Abreu     79            
#>  2    596847 Ji Man Choi       /api… Ji Man     Choi      26            
#>  3    666915 Bobby Dalbec      /api… Robert     Dalbec    29            
#>  4    665489 Vladimir Guerrer… /api… Vladimir   Guerrero  27            
#>  5    493329 Yuli Gurriel      /api… Yulieski   Gurriel   10            
#>  6    663993 Nathaniel Lowe    /api… David      Lowe      31            
#>  7    641820 Trey Mancini      /api… Joseph     Mancini   34            
#>  8    647304 Josh Naylor       /api… Joshua-Do… Naylor    12            
#>  9    621566 Matt Olson        /api… Matthew    Olson     28            
#> 10    593934 Miguel Sanó       /api… Miguel     Sanó      22            
#> # ℹ 126 more rows
#> # ℹ 43 more variables: birth_date <chr>, current_age <int>,
#> #   birth_city <chr>, birth_country <chr>, height <chr>, weight <int>,
#> #   active <lgl>, use_name <chr>, use_last_name <chr>,
#> #   middle_name <chr>, boxscore_name <chr>, nick_name <chr>,
#> #   gender <chr>, name_matrilineal <chr>, is_player <lgl>,
#> #   is_verified <lgl>, pronunciation <chr>, last_played_date <chr>, …
# }
```

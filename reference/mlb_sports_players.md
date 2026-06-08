# **MLB Sport Players**

**MLB Sport Players**

## Usage

``` r
mlb_sports_players(sport_id = 1, season = 2021)
```

## Arguments

- sport_id:

  The sport_id to return information for.

- season:

  The season to return information for.

## Value

Returns a tibble with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | integer | MLBAM player ID. |
| full_name | character | Player full name. |
| link | character | API link to the player resource. |
| first_name | character | Player first name. |
| last_name | character | Player last name. |
| primary_number | character | Primary uniform number. |
| birth_date | character | Date of birth (YYYY-MM-DD). |
| current_age | integer | Current age in years. |
| birth_city | character | City of birth. |
| birth_country | character | Country of birth. |
| height | character | Listed height (feet and inches). |
| weight | integer | Listed weight in pounds. |
| active | logical | Whether the player is currently active. |
| use_name | character | Preferred first name for display. |
| use_last_name | character | Preferred last name for display. |
| middle_name | character | Player middle name. |
| boxscore_name | character | Name as shown in box scores. |
| nick_name | character | Player nickname. |
| gender | character | Player gender code. |
| is_player | logical | Whether the person is a player. |
| is_verified | logical | Whether the profile is verified. |
| pronunciation | character | Phonetic pronunciation of the name. |
| mlb_debut_date | character | MLB debut date (YYYY-MM-DD). |
| name_first_last | character | Name in first-last order. |
| name_slug | character | URL slug for the player. |
| first_last_name | character | First and last name display. |
| last_first_name | character | Last, first name display. |
| last_init_name | character | Last name with first initial. |
| init_last_name | character | First initial with last name. |
| full_fml_name | character | Full first-middle-last name. |
| full_lfm_name | character | Full last-first-middle name. |
| strike_zone_top | numeric | Top of the player's strike zone (feet). |
| strike_zone_bottom | numeric | Bottom of the player's strike zone (feet). |
| birth_state_province | character | State or province of birth. |
| draft_year | integer | Year the player was drafted. |
| name_matrilineal | character | Matrilineal (maternal) surname. |
| last_played_date | character | Date of last MLB appearance. |
| name_title | character | Name title prefix. |
| name_suffix | character | Name suffix (e.g., Jr., III). |
| death_date | character | Date of death (YYYY-MM-DD). |
| death_city | character | City of death. |
| death_country | character | Country of death. |
| current_team_id | integer | Current team MLBAM ID. |
| current_team_name | character | Current team name. |
| current_team_link | character | API link to the current team. |
| primary_position_code | character | Primary position code. |
| primary_position_name | character | Primary position name. |
| primary_position_type | character | Primary position type. |
| primary_position_abbreviation | character | Primary position abbreviation. |
| bat_side_code | character | Batting side code (L/R/S). |
| bat_side_description | character | Batting side description. |
| pitch_hand_code | character | Throwing hand code (L/R). |
| pitch_hand_description | character | Throwing hand description. |

## Examples

``` r
# \donttest{
  try(mlb_sports_players(sport_id = 1, season = 2021))
#> ── MLB Sports - Players data from MLB.com ─────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 11:09:26 UTC
#> # A tibble: 1,508 × 53
#>    player_id full_name        link   first_name last_name primary_number
#>        <int> <chr>            <chr>  <chr>      <chr>     <chr>         
#>  1    472551 Fernando Abad    /api/… Fernando   Abad      58            
#>  2    676265 Cory Abbott      /api/… Cory       Abbott    15            
#>  3    656061 Albert Abreu     /api/… Albert     Abreu     84            
#>  4    650556 Bryan Abreu      /api/… Bryan      Abreu     66            
#>  5    547989 José Abreu       /api/… José       Abreu     79            
#>  6    642758 Domingo Acevedo  /api/… Domingo    Acevedo   68            
#>  7    660670 Ronald Acuña Jr. /api/… Ronald     Acuña     13            
#>  8    592094 Jason Adam       /api/… Jason      Adam      60            
#>  9    642715 Willy Adames     /api/… Willy      Adames    27            
#> 10    613534 Austin Adams     /api/… Austin     Adams     54            
#> # ℹ 1,498 more rows
#> # ℹ 47 more variables: birth_date <chr>, current_age <int>,
#> #   birth_city <chr>, birth_country <chr>, height <chr>, weight <int>,
#> #   active <lgl>, use_name <chr>, use_last_name <chr>,
#> #   middle_name <chr>, boxscore_name <chr>, nick_name <chr>,
#> #   gender <chr>, is_player <lgl>, is_verified <lgl>,
#> #   pronunciation <chr>, mlb_debut_date <chr>, name_first_last <chr>, …
# }
```

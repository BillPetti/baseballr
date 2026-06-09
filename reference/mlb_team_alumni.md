# **MLB Team Alumni**

**MLB Team Alumni**

## Usage

``` r
mlb_team_alumni(team_id = NULL, stat_group = NULL, season = NULL)
```

## Arguments

- team_id:

  Team ID to return information and ranking for a particular statistic
  for a particular team.

- stat_group:

  Stat group to return information and ranking for a particular
  statistic in a particular group.

- season:

  Year to return information and ranking for a particular statistic in a
  given year.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | integer | MLBAM player ID. |
| player_full_name | character | Player full name. |
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
| name_matrilineal | character | Matrilineal (maternal) surname. |
| is_player | logical | Whether the person is a player. |
| is_verified | logical | Whether the profile is verified. |
| pronunciation | character | Phonetic pronunciation of the name. |
| last_played_date | character | Date of last MLB appearance. |
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
| alumni_last_season | character | Last season the player was with the team. |
| birth_state_province | character | State or province of birth. |
| draft_year | integer | Year the player was drafted. |
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
  try(mlb_team_alumni(team_id = 137, stat_group = 'hitting', season = 2021))
#> ── MLB Team Alumni data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-09 20:44:02 UTC
#> # A tibble: 43 × 46
#>    player_id player_full_name  link  first_name last_name primary_number
#>        <int> <chr>             <chr> <chr>      <chr>     <chr>         
#>  1    501303 Ehire Adrianza    /api… Ehire      Adrianza  6             
#>  2    542881 Tyler Anderson    /api… Tyler      Anderson  31            
#>  3    641312 Shaun Anderson    /api… Shaun      Anderson  64            
#>  4    624414 Christian Arroyo  /api… Christian  Arroyo    39            
#>  5    623214 Abiatal Avelino   /api… Abiatal    Avelino   NA            
#>  6    642772 Luis Alexander B… /api… Luis       Basabe    NA            
#>  7    542963 Rob Brantly       /api… Robert     Brantly   NA            
#>  8    518516 Madison Bumgarner /api… Madison    Bumgarner 40            
#>  9    502239 Trevor Cahill     /api… Trevor     Cahill    NA            
#> 10    593525 Orlando Calixte   /api… Orlando    Calixte   16            
#> # ℹ 33 more rows
#> # ℹ 40 more variables: birth_date <chr>, current_age <int>,
#> #   birth_city <chr>, birth_country <chr>, height <chr>, weight <int>,
#> #   active <lgl>, use_name <chr>, use_last_name <chr>,
#> #   middle_name <chr>, boxscore_name <chr>, nick_name <chr>,
#> #   gender <chr>, name_matrilineal <chr>, is_player <lgl>,
#> #   is_verified <lgl>, pronunciation <chr>, last_played_date <chr>, …
# }
```

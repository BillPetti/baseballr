# **Retrieve draft pick information by year**

**Retrieve draft pick information by year**

## Usage

``` r
mlb_draft(year)
```

## Arguments

- year:

  The year for which to return data

## Value

Returns a tibble with information for every draft pick in every round
for the year requested

|  |  |  |
|----|----|----|
| col_name | types | description |
| bis_player_id | integer | BIS (Baseball Info Solutions) player ID. |
| pick_round | character | Draft round. |
| pick_number | integer | Overall pick number. |
| display_pick_number | integer | Pick number as displayed. |
| round_pick_number | integer | Pick number within the round. |
| rank | integer | Prospect rank. |
| pick_value | character | Assigned slot/pick value (dollars). |
| signing_bonus | character | Signing bonus (dollars). |
| scouting_report | character | Link to the scouting report. |
| blurb | character | Prospect scouting blurb. |
| headshot_link | character | URL to the player's headshot image. |
| is_drafted | logical | Whether the prospect was drafted. |
| is_pass | logical | Whether the pick was a pass. |
| year | character | Draft year (YYYY). |
| home_city | character | Prospect home city. |
| home_state | character | Prospect home state. |
| home_country | character | Prospect home country. |
| school_name | character | School name. |
| school_school_class | character | School class (e.g. 4YR JR, HS SR). |
| school_country | character | School country. |
| school_state | character | School state. |
| person_id | integer | MLB player ID. |
| person_full_name | character | Player full name. |
| person_link | character | MLB Stats API relative player link. |
| person_first_name | character | Player first name. |
| person_last_name | character | Player last name. |
| person_primary_number | character | Player uniform number. |
| person_birth_date | character | Birth date (YYYY-MM-DD). |
| person_current_age | integer | Current age in years. |
| person_birth_city | character | City of birth. |
| person_birth_state_province | character | State or province of birth. |
| person_birth_country | character | Country of birth. |
| person_height | character | Height (feet and inches). |
| person_weight | integer | Weight in pounds. |
| person_active | logical | Whether the player is currently active. |
| person_use_name | character | Preferred first name. |
| person_use_last_name | character | Preferred last name. |
| person_middle_name | character | Player middle name. |
| person_boxscore_name | character | Name as shown in box scores. |
| person_gender | character | Player gender. |
| person_is_player | logical | Whether the person is a player. |
| person_is_verified | logical | Whether the player profile is verified. |
| person_draft_year | integer | Year the player was drafted. |
| person_mlb_debut_date | character | MLB debut date (YYYY-MM-DD). |
| person_name_first_last | character | Name in first-last order. |
| person_name_slug | character | URL-friendly name slug. |
| person_first_last_name | character | First and last name. |
| person_last_first_name | character | Name in last, first order. |
| person_last_init_name | character | Last name with first initial. |
| person_init_last_name | character | First initial with last name. |
| person_full_fml_name | character | Full name (first-middle-last). |
| person_full_lfm_name | character | Full name (last-first-middle). |
| person_strike_zone_top | numeric | Top of the player's strike zone (feet). |
| person_strike_zone_bottom | numeric | Bottom of the player's strike zone (feet). |
| person_pronunciation | character | Phonetic name pronunciation. |
| person_name_title | character | Name title. |
| person_name_suffix | character | Name suffix (e.g. Jr., III). |
| person_nick_name | character | Player nickname. |
| person_name_matrilineal | character | Maternal family name. |
| person_primary_position_code | character | Primary fielding position code. |
| person_primary_position_name | character | Primary fielding position name. |
| person_primary_position_type | character | Primary position type (e.g. Infielder). |
| person_primary_position_abbreviation | character | Primary position abbreviation. |
| person_bat_side_code | character | Batting side code (L/R/S). |
| person_bat_side_description | character | Batting side description. |
| person_pitch_hand_code | character | Throwing hand code (L/R). |
| person_pitch_hand_description | character | Throwing hand description. |
| team_all_star_status | character | Team all-star status flag. |
| team_id | integer | MLB team ID of the drafting team. |
| team_name | character | Drafting team name. |
| team_link | character | MLB Stats API relative team link. |
| team_spring_league_id | integer | Spring training league ID. |
| team_spring_league_name | character | Spring training league name. |
| team_spring_league_link | character | MLB Stats API relative spring league link. |
| team_spring_league_abbreviation | character | Spring training league abbreviation. |
| draft_type_code | character | Draft type code. |
| draft_type_description | character | Draft type description. |

## Examples

``` r
# \donttest{
  try(mlb_draft(year = 2020))
#> ── MLB Draft data from MLB.com ────────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:40:39 UTC
#> # A tibble: 160 × 77
#>    bis_player_id pick_round pick_number display_pick_number
#>            <int> <chr>            <int>               <int>
#>  1        807255 1                    1                   1
#>  2        788201 1                    2                   2
#>  3        792606 1                    3                   3
#>  4        771881 1                    4                   4
#>  5        415542 1                    5                   5
#>  6        784616 1                    6                   6
#>  7       5009176 1                    7                   7
#>  8        807429 1                    8                   8
#>  9       5005747 1                    9                   9
#> 10        780070 1                   10                  10
#> # ℹ 150 more rows
#> # ℹ 73 more variables: round_pick_number <int>, rank <int>,
#> #   pick_value <chr>, signing_bonus <chr>, scouting_report <chr>,
#> #   blurb <chr>, headshot_link <chr>, is_drafted <lgl>, is_pass <lgl>,
#> #   year <chr>, home_city <chr>, home_state <chr>, home_country <chr>,
#> #   school_name <chr>, school_school_class <chr>, school_country <chr>,
#> #   school_state <chr>, person_id <int>, person_full_name <chr>, …
# }
```

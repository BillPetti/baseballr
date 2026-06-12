# **Retrieve draft prospect information by year**

**Retrieve draft prospect information by year**

## Usage

``` r
mlb_draft_prospects(year)
```

## Arguments

- year:

  The year for which to return data

## Value

Returns a tibble with information for every draft prospect for the year
requested:

|  |  |  |
|----|----|----|
| col_name | types | description |
| bis_player_id | integer | BIS (Baseball Info Solutions) player ID. |
| headshot_link | character | URL to the player's headshot image. |
| is_drafted | logical | Whether the prospect was drafted. |
| is_pass | logical | Whether the pick was a pass. |
| year | character | Draft year (YYYY). |
| pick_round | character | Draft round. |
| pick_number | integer | Overall pick number. |
| rank | integer | Prospect rank. |
| scouting_report | character | Link to the scouting report. |
| blurb | character | Prospect scouting blurb. |
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
| person_primary_number | character | Player uniform number. |
| person_mlb_debut_date | character | MLB debut date (YYYY-MM-DD). |
| person_pronunciation | character | Phonetic name pronunciation. |
| person_name_matrilineal | character | Maternal family name. |
| person_name_title | character | Name title. |
| person_name_suffix | character | Name suffix (e.g. Jr., III). |
| person_nick_name | character | Player nickname. |
| person_death_date | character | Date of death (YYYY-MM-DD), if applicable. |
| person_death_city | character | City of death, if applicable. |
| person_death_state_province | character | State or province of death, if applicable. |
| person_death_country | character | Country of death, if applicable. |
| person_primary_position_code | character | Primary fielding position code. |
| person_primary_position_name | character | Primary fielding position name. |
| person_primary_position_type | character | Primary position type (e.g. Infielder). |
| person_primary_position_abbreviation | character | Primary position abbreviation. |
| person_bat_side_code | character | Batting side code (L/R/S). |
| person_bat_side_description | character | Batting side description. |
| person_pitch_hand_code | character | Throwing hand code (L/R). |
| person_pitch_hand_description | character | Throwing hand description. |
| draft_type_code | character | Draft type code. |
| draft_type_description | character | Draft type description. |
| team_all_star_status | character | Team all-star status flag. |
| team_id | integer | MLB team ID associated with the prospect. |
| team_name | character | Team name. |
| team_link | character | MLB Stats API relative team link. |
| team_season | integer | Team season (YYYY). |
| team_team_code | character | Team code. |
| team_file_code | character | Team file code. |
| team_abbreviation | character | Team abbreviation. |
| team_team_name | character | Team nickname. |
| team_location_name | character | Team location/city name. |
| team_first_year_of_play | character | First year the franchise played. |
| team_short_name | character | Team short name. |
| team_franchise_name | character | Franchise name. |
| team_club_name | character | Club name. |
| team_active | logical | Whether the team is currently active. |
| team_spring_league_id | integer | Spring training league ID. |
| team_spring_league_name | character | Spring training league name. |
| team_spring_league_link | character | MLB Stats API relative spring league link. |
| team_spring_league_abbreviation | character | Spring training league abbreviation. |
| team_venue_id | integer | Home venue ID. |
| team_venue_name | character | Home venue name. |
| team_venue_link | character | MLB Stats API relative venue link. |
| team_spring_venue_id | integer | Spring training venue ID. |
| team_spring_venue_link | character | MLB Stats API relative spring venue link. |
| team_league_id | integer | MLB league ID. |
| team_league_name | character | League name. |
| team_league_link | character | MLB Stats API relative league link. |
| team_division_id | integer | Division ID. |
| team_division_name | character | Division name. |
| team_division_link | character | MLB Stats API relative division link. |
| team_sport_id | integer | MLB sport ID. |
| team_sport_link | character | MLB Stats API relative sport link. |
| team_sport_name | character | Sport name. |

## Examples

``` r
# \donttest{
  try(mlb_draft_prospects(year = 2020))
#> ── MLB Draft Prospects data from MLB.com ──────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 13:46:17 UTC
#> # A tibble: 2,554 × 102
#>    bis_player_id headshot_link       is_drafted is_pass year  pick_round
#>            <int> <chr>               <lgl>      <lgl>   <chr> <chr>     
#>  1       5012729 https://img.mlbsta… FALSE      FALSE   2020  NA        
#>  2        804300 https://img.mlbsta… FALSE      FALSE   2020  NA        
#>  3       5010766 https://img.mlbsta… FALSE      FALSE   2020  NA        
#>  4       5009176 https://img.mlbsta… TRUE       FALSE   2020  1         
#>  5            NA https://img.mlbsta… TRUE       FALSE   2020  1         
#>  6       5012222 https://img.mlbsta… FALSE      FALSE   2020  NA        
#>  7       5002839 https://img.mlbsta… FALSE      FALSE   2020  NA        
#>  8       5001752 https://img.mlbsta… FALSE      FALSE   2020  NA        
#>  9        767407 https://img.mlbsta… FALSE      FALSE   2020  NA        
#> 10        802464 https://img.mlbsta… FALSE      FALSE   2020  NA        
#> # ℹ 2,544 more rows
#> # ℹ 96 more variables: pick_number <int>, rank <int>,
#> #   scouting_report <chr>, blurb <chr>, home_city <chr>,
#> #   home_state <chr>, home_country <chr>, school_name <chr>,
#> #   school_school_class <chr>, school_country <chr>,
#> #   school_state <chr>, person_id <int>, person_full_name <chr>,
#> #   person_link <chr>, person_first_name <chr>, …
# }
```

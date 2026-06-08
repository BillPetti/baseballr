# **Retrieve Homerun Derby Players**

**Retrieve Homerun Derby Players**

## Usage

``` r
mlb_homerun_derby_players(game_pk)
```

## Arguments

- game_pk:

  The game_pk for which you want to return data

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_pk | integer | MLB game primary key for the Home Run Derby event. |
| event_name | character | Event name (e.g. 'All-Star Workout Day: Home Run Derby'). |
| event_date | character | Event date-time in ISO 8601 (e.g. '2017-07-11T00:00:00Z'). |
| event_type_code | character | Single-letter event type code (e.g. 'O'). |
| event_type_name | character | Event type name (e.g. 'Other'). |
| venue_id | integer | MLB venue id hosting the event. |
| venue_name | character | Venue name (e.g. 'Marlins Park'). |
| player_id | integer | MLB player id of the participant. |
| player_full_name | character | Participant full name. |
| player_link | character | API relative link to the player. |
| player_first_name | character | Participant first name. |
| player_last_name | character | Participant last name. |
| player_primary_number | character | Participant primary jersey number. |
| player_birth_date | character | Participant birth date (YYYY-MM-DD). |
| player_current_age | integer | Participant current age in years. |
| player_birth_city | character | Participant birth city. |
| player_birth_state_province | character | Participant birth state or province. |
| player_birth_country | character | Participant birth country. |
| player_height | character | Participant height (e.g. "6' 5\\"). |
| player_weight | integer | Participant weight in pounds. |
| player_active | logical | Whether the participant is currently an active player. |
| player_use_name | character | Participant preferred display first name. |
| player_middle_name | character | Participant middle name. |
| player_boxscore_name | character | Participant short box score name. |
| player_nick_name | character | Participant nickname. |
| player_gender | character | Participant gender code (e.g. 'M'). |
| player_is_player | logical | Whether the person is classified as a player. |
| player_is_verified | logical | Whether the player profile is verified. |
| player_draft_year | integer | Year the participant was drafted. |
| player_pronunciation | character | Phonetic pronunciation of the participant's name. |
| player_mlb_debut_date | character | Participant MLB debut date (YYYY-MM-DD). |
| player_name_first_last | character | Participant name in first-last order. |
| player_name_slug | character | URL slug for the participant (name plus id). |
| player_first_last_name | character | Participant name in first-last order. |
| player_last_first_name | character | Participant name in last, first order. |
| player_last_init_name | character | Participant name as last, first initial. |
| player_init_last_name | character | Participant name as first initial last. |
| player_full_fml_name | character | Participant full first-middle-last name. |
| player_full_lfm_name | character | Participant full last, first-middle name. |
| player_strike_zone_top | numeric | Top of the participant's strike zone (feet). |
| player_strike_zone_bottom | numeric | Bottom of the participant's strike zone (feet). |
| player_name_matrilineal | character | Participant matrilineal name when provided. |
| player_current_team_id | integer | Participant current team id. |
| player_current_team_name | character | Participant current team name. |
| player_current_team_link | character | API relative link to the current team. |
| player_current_team_season | integer | Season of the current team reference. |
| player_current_team_team_code | character | Current team three-letter team code. |
| player_current_team_file_code | character | Current team file code. |
| player_current_team_abbreviation | character | Current team abbreviation (e.g. 'NYY'). |
| player_current_team_team_name | character | Current team short team name (e.g. 'Yankees'). |
| player_current_team_location_name | character | Current team location name. |
| player_current_team_first_year_of_play | character | Franchise first year of play. |
| player_current_team_short_name | character | Current team short name. |
| player_current_team_franchise_name | character | Current team franchise name. |
| player_current_team_club_name | character | Current team club name. |
| player_current_team_all_star_status | character | Current team all-star status flag. |
| player_current_team_active | logical | Whether the current team is active. |
| player_current_team_parent_org_name | character | Parent organization name (minors). |
| player_current_team_parent_org_id | integer | Parent organization id (minors). |
| player_current_team_venue_id | integer | Current team home venue id. |
| player_current_team_venue_name | character | Current team home venue name. |
| player_current_team_venue_link | character | API relative link to the team venue. |
| player_current_team_spring_venue_id | integer | Current team spring training venue id. |
| player_current_team_spring_venue_link | character | API relative link to the spring venue. |
| player_current_team_league_id | integer | Current team league id (e.g. 103, 104). |
| player_current_team_league_name | character | Current team league name. |
| player_current_team_league_link | character | API relative link to the league. |
| player_current_team_division_id | integer | Current team division id. |
| player_current_team_division_name | character | Current team division name. |
| player_current_team_division_link | character | API relative link to the division. |
| player_current_team_sport_id | integer | Current team sport id (1 for MLB). |
| player_current_team_sport_link | character | API relative link to the sport. |
| player_current_team_sport_name | character | Current team sport name. |
| player_current_team_spring_league_id | integer | Spring league id. |
| player_current_team_spring_league_name | character | Spring league name (e.g. 'Grapefruit League'). |
| player_current_team_spring_league_link | character | API relative link to the spring league. |
| player_current_team_spring_league_abbreviation | character | Spring league abbreviation (e.g. 'GL'). |
| player_primary_position_code | character | Participant primary position code. |
| player_primary_position_name | character | Participant primary position name. |
| player_primary_position_type | character | Participant primary position type (e.g. 'Hitter'). |
| player_primary_position_abbreviation | character | Participant primary position abbreviation (e.g. 'DH'). |
| player_bat_side_code | character | Participant batting side code (e.g. 'R'). |
| player_bat_side_description | character | Participant batting side description (e.g. 'Right'). |
| player_pitch_hand_code | character | Participant throwing hand code (e.g. 'R'). |
| player_pitch_hand_description | character | Participant throwing hand description (e.g. 'Right'). |
| venue_link | character | API relative link to the event venue. |
| is_multi_day | logical | Whether the event spans multiple days. |
| is_primary_calendar | logical | Whether the event is on the primary calendar. |
| file_code | character | Internal file code for the event. |
| event_number | integer | Event number identifier. |
| public_facing | logical | Whether the event is public facing. |

## Examples

``` r
# \donttest{
  try(mlb_homerun_derby_players(game_pk = 511101))
#> ── MLB Homerun Derby Players data from MLB.com ────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:45:55 UTC
#> # A tibble: 8 × 91
#>   game_pk event_name event_date event_type_code event_type_name venue_id
#>     <int> <chr>      <chr>      <chr>           <chr>              <int>
#> 1  511101 All-Star … 2017-07-1… O               Other               4169
#> 2  511101 All-Star … 2017-07-1… O               Other               4169
#> 3  511101 All-Star … 2017-07-1… O               Other               4169
#> 4  511101 All-Star … 2017-07-1… O               Other               4169
#> 5  511101 All-Star … 2017-07-1… O               Other               4169
#> 6  511101 All-Star … 2017-07-1… O               Other               4169
#> 7  511101 All-Star … 2017-07-1… O               Other               4169
#> 8  511101 All-Star … 2017-07-1… O               Other               4169
#> # ℹ 85 more variables: venue_name <chr>, player_id <int>,
#> #   player_full_name <chr>, player_link <chr>, player_first_name <chr>,
#> #   player_last_name <chr>, player_primary_number <chr>,
#> #   player_birth_date <chr>, player_current_age <int>,
#> #   player_birth_city <chr>, player_birth_state_province <chr>,
#> #   player_birth_country <chr>, player_height <chr>,
#> #   player_weight <int>, player_active <lgl>, player_use_name <chr>, …
# }
```

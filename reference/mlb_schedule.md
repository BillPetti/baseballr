# **Find game_pk values for professional baseball games (major and minor leagues)**

**Find game_pk values for professional baseball games (major and minor
leagues)**

## Usage

``` r
mlb_schedule(season = 2019, level_ids = "1")
```

## Arguments

- season:

  The season for which you want to find game_pk values for MLB games

- level_ids:

  A numeric vector with ids for each level where game_pks are desired.
  See below for a reference of level ids.

  |  |  |  |  |  |  |  |
  |----|----|----|----|----|----|----|
  | sport_id | sport_code | sport_link | sport_name | sport_abbreviation | sort_order | active_status |
  | 1 | mlb | /api/v1/sports/1 | Major League Baseball | MLB | 11 | TRUE |
  | 11 | aaa | /api/v1/sports/11 | Triple-A | AAA | 101 | TRUE |
  | 12 | aax | /api/v1/sports/12 | Double-A | AA | 201 | TRUE |
  | 13 | afa | /api/v1/sports/13 | High-A | A+ | 301 | TRUE |
  | 14 | afx | /api/v1/sports/14 | Low-A | A | 401 | TRUE |
  | 16 | rok | /api/v1/sports/16 | Rookie | ROK | 701 | TRUE |
  | 17 | win | /api/v1/sports/17 | Winter Leagues | WIN | 1301 | TRUE |
  | 8 | bbl | /api/v1/sports/8 | Organized Baseball | Pros | 1401 | TRUE |
  | 21 | min | /api/v1/sports/21 | Minor League Baseball | Minors | 1402 | TRUE |
  | 23 | ind | /api/v1/sports/23 | Independent Leagues | IND | 2101 | TRUE |
  | 51 | int | /api/v1/sports/51 | International Baseball | INT | 3501 | TRUE |
  | 508 | nat | /api/v1/sports/508 | International Baseball (Collegiate) | INTC | 3502 | TRUE |
  | 509 | nae | /api/v1/sports/509 | International Baseball (18 and under) | 18U | 3503 | TRUE |
  | 510 | nas | /api/v1/sports/510 | International Baseball (16 and under) | 16U | 3505 | TRUE |
  | 22 | bbc | /api/v1/sports/22 | College Baseball | College | 5101 | TRUE |
  | 586 | hsb | /api/v1/sports/586 | High School Baseball | H.S. | 6201 | TRUE |

## Value

Returns a tibble which includes `game_pk` values and additional
information for games scheduled or played with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| date | character | Calendar date for the schedule entry. |
| total_items | integer | Total schedule items on the date. |
| total_events | integer | Total non-game events on the date. |
| total_games | integer | Total games on the date. |
| total_games_in_progress | integer | Games currently in progress on the date. |
| game_pk | integer | Unique game identifier. |
| game_guid | character | Globally unique game identifier (GUID). |
| link | character | API link to the game feed. |
| game_type | character | Game type code (e.g. 'R', 'S', 'W'). |
| season | character | Season the game belongs to. |
| game_date | character | Game date-time in UTC (ISO 8601). |
| official_date | character | Official game date (YYYY-MM-DD). |
| game_number | integer | Game number within a doubleheader. |
| public_facing | logical | Whether the game is public-facing. |
| double_header | character | Doubleheader indicator ('N', 'S', 'Y'). |
| gameday_type | character | Gameday data feed type. |
| tiebreaker | character | Whether the game is a tiebreaker. |
| calendar_event_id | character | Calendar event identifier. |
| season_display | character | Display string for the season. |
| day_night | character | Day or night game indicator. |
| scheduled_innings | integer | Scheduled number of innings. |
| reverse_home_away_status | logical | Whether home/away teams are reversed. |
| inning_break_length | integer | Length of inning breaks in seconds. |
| games_in_series | integer | Number of games in the series. |
| series_game_number | integer | Game number within the series. |
| series_description | character | Description of the series. |
| record_source | character | Source of the schedule record. |
| if_necessary | character | Whether the game is played only if necessary. |
| if_necessary_description | character | Description of the if-necessary status. |
| status_abstract_game_state | character | Abstract game state (e.g. 'Final'). |
| status_coded_game_state | character | Coded game state. |
| status_detailed_state | character | Detailed game state (e.g. 'Cancelled'). |
| status_status_code | character | Status code for the game. |
| status_start_time_tbd | logical | Whether the start time is TBD. |
| status_reason | character | Reason for the game status (e.g. 'Rain'). |
| status_abstract_game_code | character | Abstract game state code. |
| teams_away_split_squad | logical | Whether the away team is a split squad. |
| teams_away_series_number | integer | Away team's series number. |
| teams_away_team_id | integer | Away team MLBAM ID. |
| teams_away_team_name | character | Away team name. |
| teams_away_team_link | character | API link to the away team. |
| teams_away_league_record_wins | integer | Away team league-record wins. |
| teams_away_league_record_losses | integer | Away team league-record losses. |
| teams_away_league_record_ties | integer | Away team league-record ties. |
| teams_away_league_record_pct | character | Away team winning percentage. |
| teams_home_split_squad | logical | Whether the home team is a split squad. |
| teams_home_series_number | integer | Home team's series number. |
| teams_home_team_id | integer | Home team MLBAM ID. |
| teams_home_team_name | character | Home team name. |
| teams_home_team_link | character | API link to the home team. |
| teams_home_league_record_wins | integer | Home team league-record wins. |
| teams_home_league_record_losses | integer | Home team league-record losses. |
| teams_home_league_record_ties | integer | Home team league-record ties. |
| teams_home_league_record_pct | character | Home team winning percentage. |
| venue_id | integer | MLBAM venue ID. |
| venue_name | character | Venue name. |
| venue_link | character | API link to the venue. |
| content_link | character | API link to the game content. |
| is_tie | logical | Whether the game ended in a tie. |
| description | character | Game description (often for exhibition games). |
| teams_away_score | integer | Away team final score. |
| teams_away_is_winner | logical | Whether the away team won. |
| teams_home_score | integer | Home team final score. |
| teams_home_is_winner | logical | Whether the home team won. |
| reschedule_date | character | Reschedule date-time, if rescheduled. |
| reschedule_game_date | character | Reschedule game date, if rescheduled. |
| rescheduled_from | character | Original date-time the game was rescheduled from. |
| rescheduled_from_date | character | Original date the game was rescheduled from. |
| resume_date | character | Resume date-time, if suspended/resumed. |
| resume_game_date | character | Resume game date, if suspended/resumed. |
| resumed_from | character | Date-time the game was resumed from. |
| resumed_from_date | character | Date the game was resumed from. |
| events | list | Nested list of non-game events. |

## Level IDs

The following IDs can be passed to the level_ids argument:

1 = MLB  
11 = Triple-A  
12 = Doubl-A  
13 = Class A Advanced  
14 = Class A  
15 = Class A Short Season  
5442 = Rookie Advanced  
16 = Rookie  
17 = Winter League  

## Examples

``` r
# \donttest{
  try(mlb_schedule(season = "2019"))
#> ── MLB Schedule data from MLB.com ─────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:25:05 UTC
#> # A tibble: 3,010 × 73
#>    date      total_items total_events total_games total_games_in_progr…¹
#>    <chr>           <int>        <int>       <int>                  <int>
#>  1 2019-02-…           1            0           1                      0
#>  2 2019-02-…           4            0           4                      0
#>  3 2019-02-…           4            0           4                      0
#>  4 2019-02-…           4            0           4                      0
#>  5 2019-02-…           4            0           4                      0
#>  6 2019-02-…          16            0          16                      0
#>  7 2019-02-…          16            0          16                      0
#>  8 2019-02-…          16            0          16                      0
#>  9 2019-02-…          16            0          16                      0
#> 10 2019-02-…          16            0          16                      0
#> # ℹ 3,000 more rows
#> # ℹ abbreviated name: ¹​total_games_in_progress
#> # ℹ 68 more variables: game_pk <int>, game_guid <chr>, link <chr>,
#> #   game_type <chr>, season <chr>, game_date <chr>,
#> #   official_date <chr>, game_number <int>, public_facing <lgl>,
#> #   double_header <chr>, gameday_type <chr>, tiebreaker <chr>,
#> #   calendar_event_id <chr>, season_display <chr>, day_night <chr>, …
# }
```

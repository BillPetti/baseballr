# **Find game_pk values for professional baseball games (major and minor leagues) that are tied**

**Find game_pk values for professional baseball games (major and minor
leagues) that are tied**

## Usage

``` r
mlb_schedule_games_tied(season = 2021, game_type = "S")
```

## Arguments

- season:

  The season for which you want to find game_pk values for MLB games

- game_type:

  game_type to return schedule information for all tied games in a
  particular game_type

  |              |                            |
  |--------------|----------------------------|
  | game_type_id | game_type_description      |
  | S            | Spring Training            |
  | R            | Regular Season             |
  | F            | Wild Card Game             |
  | D            | Division Series            |
  | L            | League Championship Series |
  | W            | World Series               |
  | C            | Championship               |
  | N            | Nineteenth Century Series  |
  | P            | Playoffs                   |
  | A            | All-Star Game              |
  | I            | Intrasquad                 |
  | E            | Exhibition                 |

## Value

Returns a tibble that includes game_pk values and additional information
for games scheduled or played

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
| game_type | character | Game type code (e.g. 'R', 'S'). |
| season | character | Season the game belongs to. |
| game_date | character | Game date-time in UTC (ISO 8601). |
| official_date | character | Official game date (YYYY-MM-DD). |
| is_tie | logical | Whether the game ended in a tie. |
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
| games_in_series | integer | Number of games in the series. |
| series_game_number | integer | Game number within the series. |
| series_description | character | Description of the series. |
| record_source | character | Source of the schedule record. |
| if_necessary | character | Whether the game is played only if necessary. |
| if_necessary_description | character | Description of the if-necessary status. |
| status_abstract_game_state | character | Abstract game state (e.g. 'Final'). |
| status_coded_game_state | character | Coded game state. |
| status_detailed_state | character | Detailed game state. |
| status_status_code | character | Status code for the game. |
| status_start_time_tbd | logical | Whether the start time is TBD. |
| status_reason | character | Reason for the game status (e.g. 'Tied'). |
| status_abstract_game_code | character | Abstract game state code. |
| teams_away_score | integer | Away team score. |
| teams_away_split_squad | logical | Whether the away team is a split squad. |
| teams_away_series_number | integer | Away team's series number. |
| teams_away_team_id | integer | Away team MLBAM ID. |
| teams_away_team_name | character | Away team name. |
| teams_away_team_link | character | API link to the away team. |
| teams_away_league_record_wins | integer | Away team league-record wins. |
| teams_away_league_record_losses | integer | Away team league-record losses. |
| teams_away_league_record_ties | integer | Away team league-record ties. |
| teams_away_league_record_pct | character | Away team winning percentage. |
| teams_home_score | integer | Home team score. |
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
| inning_break_length | integer | Length of inning breaks in seconds. |
| events | list | Nested list of non-game events. |

## Examples

``` r
# \donttest{
  try(mlb_schedule_games_tied(season = 2021))
#> ── MLB Schedule - Games Tied data from MLB.com ────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 11:09:24 UTC
#> # A tibble: 46 × 62
#>    date      total_items total_events total_games total_games_in_progr…¹
#>    <chr>           <int>        <int>       <int>                  <int>
#>  1 2021-02-…           1            0           1                      0
#>  2 2021-03-…           4            0           4                      0
#>  3 2021-03-…           4            0           4                      0
#>  4 2021-03-…           4            0           4                      0
#>  5 2021-03-…           4            0           4                      0
#>  6 2021-03-…           3            0           3                      0
#>  7 2021-03-…           3            0           3                      0
#>  8 2021-03-…           3            0           3                      0
#>  9 2021-03-…           2            0           2                      0
#> 10 2021-03-…           2            0           2                      0
#> # ℹ 36 more rows
#> # ℹ abbreviated name: ¹​total_games_in_progress
#> # ℹ 57 more variables: game_pk <int>, game_guid <chr>, link <chr>,
#> #   game_type <chr>, season <chr>, game_date <chr>,
#> #   official_date <chr>, is_tie <lgl>, game_number <int>,
#> #   public_facing <lgl>, double_header <chr>, gameday_type <chr>,
#> #   tiebreaker <chr>, calendar_event_id <chr>, season_display <chr>, …
# }
```

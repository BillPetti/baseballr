# **Find game_pk values for professional baseball postseason series games (major and minor leagues)**

**Find game_pk values for professional baseball postseason series games
(major and minor leagues)**

## Usage

``` r
mlb_schedule_postseason_series(
  season = 2021,
  game_type = NULL,
  series_number = NULL,
  sport_id = 1,
  team_id = NULL
)
```

## Arguments

- season:

  The season for which you want to find game_pk values for MLB games

- game_type:

  game_type to return schedule information for all tied games in a
  particular game_type

- series_number:

  The Series number to return schedule information for all tied games in
  a particular series number

- sport_id:

  The sport_id to return schedule information for.

- team_id:

  The team_id to return schedule information for.

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
| total_items | integer | Total games in the series. |
| total_games | integer | Total games in the series. |
| total_games_in_progress | integer | Games currently in progress. |
| game_pk | integer | Unique game identifier. |
| game_guid | character | Globally unique game identifier (GUID). |
| link | character | API link to the game feed. |
| game_type | character | Postseason game type code (e.g. 'W', 'L', 'D'). |
| season | character | Season the game belongs to. |
| game_date | character | Game date-time in UTC (ISO 8601). |
| official_date | character | Official game date (YYYY-MM-DD). |
| is_tie | logical | Whether the game ended in a tie. |
| is_featured_game | logical | Whether the game is a featured game. |
| game_number | integer | Game number within a doubleheader. |
| public_facing | logical | Whether the game is public-facing. |
| double_header | character | Doubleheader indicator ('N', 'S', 'Y'). |
| gameday_type | character | Gameday data feed type. |
| tiebreaker | character | Whether the game is a tiebreaker. |
| calendar_event_id | character | Calendar event identifier. |
| season_display | character | Display string for the season. |
| day_night | character | Day or night game indicator. |
| description | character | Series/game description (e.g. 'World Series Game 1'). |
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
| status_detailed_state | character | Detailed game state. |
| status_status_code | character | Status code for the game. |
| status_start_time_tbd | logical | Whether the start time is TBD. |
| status_abstract_game_code | character | Abstract game state code. |
| teams_away_score | integer | Away team score. |
| teams_away_is_winner | logical | Whether the away team won. |
| teams_away_split_squad | logical | Whether the away team is a split squad. |
| teams_away_series_number | integer | Away team's series number. |
| teams_away_team_id | integer | Away team MLBAM ID. |
| teams_away_team_name | character | Away team name. |
| teams_away_team_link | character | API link to the away team. |
| teams_away_league_record_wins | integer | Away team series-record wins. |
| teams_away_league_record_losses | integer | Away team series-record losses. |
| teams_away_league_record_pct | character | Away team winning percentage. |
| teams_home_score | integer | Home team score. |
| teams_home_is_winner | logical | Whether the home team won. |
| teams_home_split_squad | logical | Whether the home team is a split squad. |
| teams_home_series_number | integer | Home team's series number. |
| teams_home_team_id | integer | Home team MLBAM ID. |
| teams_home_team_name | character | Home team name. |
| teams_home_team_link | character | API link to the home team. |
| teams_home_league_record_wins | integer | Home team series-record wins. |
| teams_home_league_record_losses | integer | Home team series-record losses. |
| teams_home_league_record_pct | character | Home team winning percentage. |
| venue_id | integer | MLBAM venue ID. |
| venue_name | character | Venue name. |
| venue_link | character | API link to the venue. |
| content_link | character | API link to the game content. |
| reschedule_date | character | Reschedule date-time, if rescheduled. |
| reschedule_game_date | character | Reschedule game date, if rescheduled. |
| rescheduled_from | character | Original date-time the game was rescheduled from. |
| rescheduled_from_date | character | Original date the game was rescheduled from. |
| status_reason | character | Reason for the game status (e.g. 'Rain'). |
| sort_order | integer | Sort order within the series listing. |
| series_id | character | Series identifier (e.g. 'W_1'). |
| series_sort_number | integer | Sort number for the series. |
| series_is_default | logical | Whether the series is the default series. |
| series_game_type | character | Game type code for the series. |

## Examples

``` r
# \donttest{
  try(mlb_schedule_postseason_series(season = 2021, sport_id = 1))
#> ── MLB Schedule - Post-season Series data from MLB.com ─────────────────
#> ℹ Data updated: 2026-06-08 03:46:10 UTC
#> # A tibble: 38 × 72
#>    total_items total_games total_games_in_progress game_pk game_guid    
#>          <int>       <int>                   <int>   <int> <chr>        
#>  1           6           6                       0  660897 c4148dd0-dc0…
#>  2           6           6                       0  660908 8644ecec-18e…
#>  3           6           6                       0  660910 404871b2-3eb…
#>  4           6           6                       0  660902 d8d931dc-494…
#>  5           6           6                       0  660911 ad3ef28d-964…
#>  6           6           6                       0  660906 35a3ffd6-2d4…
#>  7           1           1                       0  660938 42ec699b-0d4…
#>  8           1           1                       0  660937 c4cc7a37-663…
#>  9           5           5                       0  660936 99c5d8e9-125…
#> 10           5           5                       0  660934 0e6aa0d8-ec6…
#> # ℹ 28 more rows
#> # ℹ 67 more variables: link <chr>, game_type <chr>, season <chr>,
#> #   game_date <chr>, official_date <chr>, is_tie <lgl>,
#> #   is_featured_game <lgl>, game_number <int>, public_facing <lgl>,
#> #   double_header <chr>, gameday_type <chr>, tiebreaker <chr>,
#> #   calendar_event_id <chr>, season_display <chr>, day_night <chr>,
#> #   description <chr>, scheduled_innings <int>, …
# }
```

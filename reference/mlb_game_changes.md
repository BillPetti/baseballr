# **Acquire time codes for Major and Minor League games**

**Acquire time codes for Major and Minor League games**

## Usage

``` r
mlb_game_changes(updated_since, sport_id)
```

## Arguments

- updated_since:

  Updated since date time

- sport_id:

  Return division(s) for all divisions in a specific sport.

## Value

Returns a tibble that includes time codes from the game_pk requested

|  |  |  |
|----|----|----|
| col_name | types | description |
| date | character | Schedule date (YYYY-MM-DD). |
| total_items | integer | Total items for the date. |
| total_events | integer | Total events for the date. |
| total_games | integer | Total games for the date. |
| total_games_in_progress | integer | Games currently in progress for the date. |
| game_pk | integer | Unique game identifier. |
| game_guid | character | Globally unique game identifier. |
| link | character | MLB Stats API relative game link. |
| game_type | character | Game type code (R, P, D, etc.). |
| season | character | Season (YYYY). |
| game_date | character | Game date-time (ISO 8601, UTC). |
| official_date | character | Official game date (YYYY-MM-DD). |
| is_tie | logical | Whether the game ended in a tie. |
| game_number | integer | Game number within a doubleheader. |
| public_facing | logical | Whether the game is public-facing. |
| double_header | character | Doubleheader indicator (N/Y/S). |
| gameday_type | character | Gameday data type code. |
| tiebreaker | character | Tiebreaker indicator. |
| calendar_event_id | character | Calendar event identifier. |
| season_display | character | Display season (YYYY). |
| day_night | character | Day/night designation. |
| description | character | Game description. |
| scheduled_innings | integer | Number of scheduled innings. |
| reverse_home_away_status | logical | Whether home/away designation is reversed. |
| inning_break_length | integer | Length of the inning break (seconds). |
| games_in_series | integer | Total games in the series. |
| series_game_number | integer | Game number within the series. |
| series_description | character | Series description. |
| record_source | character | Source of the record data. |
| if_necessary | character | Whether the game is played only if necessary. |
| if_necessary_description | character | If-necessary description. |
| status_abstract_game_state | character | Abstract game state (e.g. Final). |
| status_coded_game_state | character | Coded game state. |
| status_detailed_state | character | Detailed game state. |
| status_status_code | character | Game status code. |
| status_start_time_tbd | logical | Whether the start time is TBD. |
| status_abstract_game_code | character | Abstract game code. |
| teams_away_score | integer | Away team score. |
| teams_away_is_winner | logical | Whether the away team won. |
| teams_away_split_squad | logical | Whether the away team is a split squad. |
| teams_away_series_number | integer | Away team series number. |
| teams_away_team_id | integer | Away team MLB ID. |
| teams_away_team_name | character | Away team name. |
| teams_away_team_link | character | MLB Stats API relative away team link. |
| teams_away_league_record_wins | integer | Away team league-record wins. |
| teams_away_league_record_losses | integer | Away team league-record losses. |
| teams_away_league_record_ties | integer | Away team league-record ties. |
| teams_away_league_record_pct | character | Away team winning percentage. |
| teams_home_score | integer | Home team score. |
| teams_home_is_winner | logical | Whether the home team won. |
| teams_home_split_squad | logical | Whether the home team is a split squad. |
| teams_home_series_number | integer | Home team series number. |
| teams_home_team_id | integer | Home team MLB ID. |
| teams_home_team_name | character | Home team name. |
| teams_home_team_link | character | MLB Stats API relative home team link. |
| teams_home_league_record_wins | integer | Home team league-record wins. |
| teams_home_league_record_losses | integer | Home team league-record losses. |
| teams_home_league_record_ties | integer | Home team league-record ties. |
| teams_home_league_record_pct | character | Home team winning percentage. |
| venue_id | integer | Venue ID. |
| venue_name | character | Venue name. |
| venue_link | character | MLB Stats API relative venue link. |
| content_link | character | MLB Stats API relative game content link. |
| status_reason | character | Reason for the game status, if any. |
| rescheduled_from | character | Original scheduled date-time if rescheduled. |
| rescheduled_from_date | character | Original scheduled date if rescheduled. |
| resumed_from | character | Original date-time if the game was resumed. |
| resumed_from_date | character | Original date if the game was resumed. |
| events | list | Nested list of change events for the game. |

## Examples

``` r
# \donttest{
  try(mlb_game_changes(updated_since = "2021-08-10T19:08:24.000004Z", sport_id = 1))
#> ── MLB Game Changes data from MLB.com ─────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 13:46:19 UTC
#> # A tibble: 1,000 × 69
#>    date      total_items total_events total_games total_games_in_progr…¹
#>    <chr>           <int>        <int>       <int>                  <int>
#>  1 1999-10-…           1            0           1                      0
#>  2 1999-10-…           1            0           1                      0
#>  3 1999-10-…           2            0           2                      0
#>  4 1999-10-…           2            0           2                      0
#>  5 1999-10-…           2            0           2                      0
#>  6 1999-10-…           2            0           2                      0
#>  7 1999-10-…           1            0           1                      0
#>  8 1999-10-…           1            0           1                      0
#>  9 1999-10-…           1            0           1                      0
#> 10 1999-10-…           1            0           1                      0
#> # ℹ 990 more rows
#> # ℹ abbreviated name: ¹​total_games_in_progress
#> # ℹ 64 more variables: game_pk <int>, game_guid <chr>, link <chr>,
#> #   game_type <chr>, season <chr>, game_date <chr>,
#> #   official_date <chr>, is_tie <lgl>, game_number <int>,
#> #   public_facing <lgl>, double_header <chr>, gameday_type <chr>,
#> #   tiebreaker <chr>, calendar_event_id <chr>, season_display <chr>, …
# }
```

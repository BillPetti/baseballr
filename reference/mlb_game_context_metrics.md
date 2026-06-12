# **Acquire game context metrics for Major and Minor League games**

**Acquire game context metrics for Major and Minor League games**

## Usage

``` r
mlb_game_context_metrics(game_pk, timecode)
```

## Arguments

- game_pk:

  The game_pk for the game requested

- timecode:

  The time code for the MLB game (format: MMDDYYYY_HHMMSS)

## Value

Returns a tibble that includes time codes from the game_pk requested

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_pk | integer | Unique game identifier. |
| game_guid | character | Globally unique game identifier. |
| link | character | MLB Stats API relative game link. |
| game_type | character | Game type code (R, P, etc.). |
| season | character | Season (YYYY). |
| game_date | character | Game date-time (ISO 8601, UTC). |
| official_date | character | Official game date (YYYY-MM-DD). |
| status_abstract_game_state | character | Abstract game state (e.g. Final). |
| status_coded_game_state | character | Coded game state. |
| status_detailed_state | character | Detailed game state. |
| status_status_code | character | Game status code. |
| status_start_time_tbd | logical | Whether the start time is TBD. |
| status_abstract_game_code | character | Abstract game code. |
| teams_away_team_id | integer | Away team MLB ID. |
| teams_away_team_name | character | Away team name. |
| teams_away_team_link | character | MLB Stats API relative away team link. |
| teams_away_league_record_wins | integer | Away team league-record wins. |
| teams_away_league_record_losses | integer | Away team league-record losses. |
| teams_away_league_record_ties | integer | Away team league-record ties. |
| teams_away_league_record_pct | character | Away team winning percentage. |
| teams_away_score | integer | Away team score. |
| teams_away_is_winner | logical | Whether the away team won. |
| teams_away_probable_pitcher_id | integer | Away probable pitcher MLB ID. |
| teams_away_probable_pitcher_full_name | character | Away probable pitcher name. |
| teams_away_probable_pitcher_link | character | MLB Stats API relative away pitcher link. |
| teams_away_split_squad | logical | Whether the away team is a split squad. |
| teams_away_series_number | integer | Away team series number. |
| teams_home_team_id | integer | Home team MLB ID. |
| teams_home_team_name | character | Home team name. |
| teams_home_team_link | character | MLB Stats API relative home team link. |
| teams_home_league_record_wins | integer | Home team league-record wins. |
| teams_home_league_record_losses | integer | Home team league-record losses. |
| teams_home_league_record_ties | integer | Home team league-record ties. |
| teams_home_league_record_pct | character | Home team winning percentage. |
| teams_home_score | integer | Home team score. |
| teams_home_is_winner | logical | Whether the home team won. |
| teams_home_probable_pitcher_id | integer | Home probable pitcher MLB ID. |
| teams_home_probable_pitcher_full_name | character | Home probable pitcher name. |
| teams_home_probable_pitcher_link | character | MLB Stats API relative home pitcher link. |
| teams_home_split_squad | logical | Whether the home team is a split squad. |
| teams_home_series_number | integer | Home team series number. |
| venue_id | integer | Venue ID. |
| venue_name | character | Venue name. |
| venue_link | character | MLB Stats API relative venue link. |
| link_1 | character | MLB Stats API relative game content link. |
| is_tie | logical | Whether the game ended in a tie. |
| game_number | integer | Game number within a doubleheader. |
| public_facing | logical | Whether the game is public-facing. |
| double_header | character | Doubleheader indicator (N/Y/S). |
| gameday_type | character | Gameday data type code. |
| tiebreaker | character | Tiebreaker indicator. |
| calendar_event_id | character | Calendar event identifier. |
| season_display | character | Display season (YYYY). |
| day_night | character | Day/night designation. |
| scheduled_innings | integer | Number of scheduled innings. |
| reverse_home_away_status | logical | Whether home/away designation is reversed. |
| inning_break_length | integer | Length of the inning break (seconds). |
| games_in_series | integer | Total games in the series. |
| series_game_number | integer | Game number within the series. |
| series_description | character | Series description. |
| record_source | character | Source of the record data. |
| if_necessary | character | Whether the game is played only if necessary. |
| if_necessary_description | character | If-necessary description. |
| game_id | character | Human-readable game ID slug. |
| home_win_probability | numeric | Home team win probability (percent). |
| away_win_probability | numeric | Away team win probability (percent). |

## Examples

``` r
# \donttest{
  try(mlb_game_context_metrics(game_pk = 531060, timecode = "20180803_182458"))
#> ── MLB Game Context Metrics data from MLB.com ─────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:24:45 UTC
#> # A tibble: 1 × 66
#>   game_pk game_guid       link  game_type season game_date official_date
#>     <int> <chr>           <chr> <chr>     <chr>  <chr>     <chr>        
#> 1  531060 053ddbf2-26e1-… /api… R         2018   2018-08-… 2018-08-03   
#> # ℹ 59 more variables: status_abstract_game_state <chr>,
#> #   status_coded_game_state <chr>, status_detailed_state <chr>,
#> #   status_status_code <chr>, status_start_time_tbd <lgl>,
#> #   status_abstract_game_code <chr>, teams_away_team_id <int>,
#> #   teams_away_team_name <chr>, teams_away_team_link <chr>,
#> #   teams_away_league_record_wins <int>,
#> #   teams_away_league_record_losses <int>, …
# }
```

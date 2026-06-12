# **Get ESPN College Baseball Scoreboard**

**Get ESPN College Baseball schedule for a specific year**

## Usage

``` r
espn_college_baseball_scoreboard(season)
```

## Arguments

- season:

  Either numeric or character (YYYYMMDD)

## Value

Returns a tibble with scoreboard data

|  |  |  |
|----|----|----|
| col_name | types | description |
| matchup | character | Full game matchup name. |
| matchup_short | character | Short matchup name. |
| season | integer | Season (4-digit year). |
| season_type | integer | ESPN season type (1=pre, 2=regular, 3=postseason, 4=off-season). |
| season_slug | character | Season slug. |
| game_id | integer | Unique ESPN game/event identifier. |
| game_uid | character | ESPN game UID. |
| game_date | Date | Game date (YYYY-MM-DD). |
| attendance | integer | Game attendance. |
| play_by_play_available | logical | TRUE if play-by-play is available. |
| was_suspended | logical | Was suspended. |
| notes | logical | Notes. |
| status_name | character | Game status (e.g. 'STATUS_FINAL'). |
| broadcast_market | character | Broadcast market ('national'/'home'/'away'). |
| broadcast_name | character | Broadcast network name. |
| batting_leader_value | numeric | Team batting: batting leader value. |
| batting_leader_stat | character | Team batting: batting leader stat. |
| batting_leader_name | character | Team batting: batting leader name. |
| batting_leader_shortname | character | Team batting: batting leader shortname. |
| batting_leader_headshot | character | Team batting: batting leader headshot. |
| batting_leader_team_id | character | Team batting: batting leader team id. |
| batting_leader_pos | character | Team batting: batting leader pos. |
| home_run_leader_value | logical | Home run leader value. |
| home_run_leader_stat | logical | Home run leader stat. |
| home_run_leader_name | logical | Home run leader name. |
| home_run_leader_shortname | logical | Home run leader shortname. |
| home_run_leader_headshot | logical | Home run leader headshot. |
| home_run_leader_team_id | logical | Home run leader team id. |
| home_run_leader_pos | logical | Home run leader pos. |
| rbi_leader_value | logical | Rbi leader value. |
| rbi_leader_stat | logical | Rbi leader stat. |
| rbi_leader_name | logical | Rbi leader name. |
| rbi_leader_shortname | logical | Rbi leader shortname. |
| rbi_leader_headshot | logical | Rbi leader headshot. |
| rbi_leader_team_id | logical | Rbi leader team id. |
| rbi_leader_pos | logical | Rbi leader pos. |
| start_date | character | Start date. |
| broadcast | character | Broadcast network. |
| highlights | logical | Highlights. |
| game_date_time | POSIXct | Game start date/time (US/Eastern). |
| home_team_name | character | Home team name. |
| home_team_logo | character | Home team logo. |
| home_team_abb | character | Home team abb. |
| home_team_id | integer | Home team id. |
| home_team_location | character | Home team location. |
| home_team_full_name | character | Home team full name. |
| home_team_color | character | Home team color. |
| home_score | integer | Home team run total after the play. |
| home_win | integer | Home win. |
| home_record | character | Home record. |
| away_team_name | character | Away team name. |
| away_team_logo | character | Away team logo. |
| away_team_abb | character | Away team abb. |
| away_team_id | integer | Away team id. |
| away_team_location | character | Away team location. |
| away_team_full_name | character | Away team full name. |
| away_team_color | character | Away team color. |
| away_score | integer | Away team run total after the play. |
| away_win | integer | Away win. |
| away_record | character | Away record. |

## See also

Other ESPN College Baseball Functions:
[`espn_college_baseball_athletes_index()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_athletes_index.md),
[`espn_college_baseball_calendar()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_calendar.md),
[`espn_college_baseball_coach()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_coach.md),
[`espn_college_baseball_coach_record()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_coach_record.md),
[`espn_college_baseball_coach_season()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_coach_season.md),
[`espn_college_baseball_coaches()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_coaches.md),
[`espn_college_baseball_conferences()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_conferences.md),
[`espn_college_baseball_game_all()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_all.md),
[`espn_college_baseball_game_broadcasts()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_broadcasts.md),
[`espn_college_baseball_game_official_detail()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_official_detail.md),
[`espn_college_baseball_game_officials()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_officials.md),
[`espn_college_baseball_game_play()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_play.md),
[`espn_college_baseball_game_play_personnel()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_play_personnel.md),
[`espn_college_baseball_game_player_box()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_player_box.md),
[`espn_college_baseball_game_rosters()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_rosters.md),
[`espn_college_baseball_game_situation()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_situation.md),
[`espn_college_baseball_game_team_leaders()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_team_leaders.md),
[`espn_college_baseball_game_team_linescores()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_team_linescores.md),
[`espn_college_baseball_game_team_records()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_team_records.md),
[`espn_college_baseball_game_team_roster()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_team_roster.md),
[`espn_college_baseball_game_team_roster_entry()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_team_roster_entry.md),
[`espn_college_baseball_game_team_score()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_team_score.md),
[`espn_college_baseball_game_team_statistics()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_game_team_statistics.md),
[`espn_college_baseball_leaders()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_leaders.md),
[`espn_college_baseball_news()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_news.md),
[`espn_college_baseball_pbp()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_pbp.md),
[`espn_college_baseball_player_awards()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_awards.md),
[`espn_college_baseball_player_box()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_box.md),
[`espn_college_baseball_player_career_stats()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_career_stats.md),
[`espn_college_baseball_player_eventlog()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_eventlog.md),
[`espn_college_baseball_player_gamelog()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_gamelog.md),
[`espn_college_baseball_player_info()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_info.md),
[`espn_college_baseball_player_overview()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_overview.md),
[`espn_college_baseball_player_seasons()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_seasons.md),
[`espn_college_baseball_player_splits()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_splits.md),
[`espn_college_baseball_player_statisticslog()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_statisticslog.md),
[`espn_college_baseball_season_group()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_group.md),
[`espn_college_baseball_season_group_children()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_group_children.md),
[`espn_college_baseball_season_group_teams()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_group_teams.md),
[`espn_college_baseball_season_groups()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_groups.md),
[`espn_college_baseball_season_info()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_info.md),
[`espn_college_baseball_season_leaders()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_leaders.md),
[`espn_college_baseball_season_ranking()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_ranking.md),
[`espn_college_baseball_season_rankings()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_rankings.md),
[`espn_college_baseball_season_type()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_type.md),
[`espn_college_baseball_season_types()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_types.md),
[`espn_college_baseball_season_week()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_week.md),
[`espn_college_baseball_season_weeks()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_season_weeks.md),
[`espn_college_baseball_seasons()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_seasons.md),
[`espn_college_baseball_standings()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_standings.md),
[`espn_college_baseball_team()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team.md),
[`espn_college_baseball_team_box()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_box.md),
[`espn_college_baseball_team_leaders()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_leaders.md),
[`espn_college_baseball_team_news()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_news.md),
[`espn_college_baseball_team_record()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_record.md),
[`espn_college_baseball_team_record_detail()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_record_detail.md),
[`espn_college_baseball_team_roster()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_roster.md),
[`espn_college_baseball_team_schedule()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_schedule.md),
[`espn_college_baseball_team_season_profile()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_season_profile.md),
[`espn_college_baseball_team_season_roster()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_season_roster.md),
[`espn_college_baseball_team_season_statistics()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_team_season_statistics.md),
[`espn_college_baseball_teams()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_teams.md),
[`espn_college_baseball_tournament()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_tournament.md),
[`espn_college_baseball_tournament_season()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_tournament_season.md),
[`espn_college_baseball_tournament_seasons()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_tournament_seasons.md),
[`espn_college_baseball_tournaments()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_tournaments.md),
[`espn_college_baseball_venues()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_venues.md),
[`espn_college_baseball_week_ranking()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_week_ranking.md),
[`espn_college_baseball_week_rankings()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_week_rankings.md)

## Author

Saiem Gilani

## Examples

``` r

# Get scoreboard from a College World Series date (2025-06-15).
# \donttest{
try(espn_college_baseball_scoreboard(season = "20250615"))
#> ── ESPN College Baseball Scoreboard Information from ESPN.com ──────────
#> ℹ Data updated: 2026-06-12 14:08:00 UTC
#> # A tibble: 2 × 37
#>   matchup  matchup_short season season_type season_slug game_id game_uid
#>   <chr>    <chr>          <int>       <int> <chr>         <int> <chr>   
#> 1 Coastal… CCU @ ORST      2025           5 world-seri…  4.02e8 s:1~l:1…
#> 2 Arizona… ARIZ @ LOU      2025           5 world-seri…  4.02e8 s:1~l:1…
#> # ℹ 30 more variables: game_date <date>, attendance <int>,
#> #   play_by_play_available <lgl>, was_suspended <lgl>,
#> #   status_name <chr>, broadcast_market <chr>, broadcast_name <chr>,
#> #   start_date <chr>, broadcast <chr>, game_date_time <dttm>,
#> #   home_team_name <chr>, home_team_logo <chr>, home_team_abb <chr>,
#> #   home_team_id <int>, home_team_location <chr>,
#> #   home_team_full_name <chr>, home_team_color <chr>, …
# }
```

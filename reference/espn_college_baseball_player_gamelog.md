# **Get ESPN College Baseball Athlete Gamelog**

**Get ESPN College Baseball Athlete Gamelog**

**Get ESPN College Baseball Athlete Gamelog**

## Usage

``` r
espn_college_baseball_player_gamelog(
  athlete_id,
  season = most_recent_college_baseball_season(),
  ...
)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- season:

  Season year (numeric). Defaults to the most recent college baseball
  season.

- ...:

  Additional arguments; currently unused.

## Value

A single tibble with one row per game. Column names reflect the stat
labels returned by ESPN and will vary by season and player.

|                       |           |                                       |
|-----------------------|-----------|---------------------------------------|
| col_name              | types     | description                           |
| id                    | character | Identifier.                           |
| at_vs                 | character | At vs.                                |
| game_date             | character | Game date (YYYY-MM-DD).               |
| score                 | character | Score.                                |
| home_team_id          | character | Unique ESPN team identifier.          |
| away_team_id          | character | Unique ESPN team identifier.          |
| home_team_score       | character | Home team score.                      |
| away_team_score       | character | Away team score.                      |
| game_result           | character | Game result (W/L).                    |
| league_name           | character | League name.                          |
| league_abbreviation   | character | League abbreviation.                  |
| league_short_name     | character | League short name.                    |
| event_note            | character | Event note.                           |
| team_id               | character | Unique ESPN team identifier.          |
| team_uid              | character | ESPN universal team identifier (UID). |
| team_abbreviation     | character | Short team abbreviation (e.g. 'NYY'). |
| team_logo             | character | Team logo.                            |
| team_is_all_star      | character | Team is all star.                     |
| opponent_id           | character | Opponent team identifier.             |
| opponent_uid          | character | ESPN UID.                             |
| opponent_display_name | character | Display name.                         |
| opponent_abbreviation | character | Opponent abbreviation.                |
| opponent_logo         | character | Opponent logo.                        |
| event_id              | character | Unique ESPN event/game identifier.    |
| week                  | character | Week.                                 |

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
[`espn_college_baseball_player_info()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_info.md),
[`espn_college_baseball_player_overview()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_overview.md),
[`espn_college_baseball_player_seasons()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_seasons.md),
[`espn_college_baseball_player_splits()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_splits.md),
[`espn_college_baseball_player_statisticslog()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_player_statisticslog.md),
[`espn_college_baseball_scoreboard()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_scoreboard.md),
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
# \donttest{
  try(espn_college_baseball_player_gamelog(athlete_id = "3730", season = 2025))
#> ✖ 2026-06-12 11:40:35.815107: Failed to retrieve ESPN college-baseball athlete gamelog for athlete_id=3730, season=2025
#> ✖ Args: league = "college-baseball", athlete_id = "3730", season = 2025
#> ✖ Error: The API returned an error, HTTP Response Code 500
#> ── ESPN COLLEGE-BASEBALL Athlete Gamelog from ESPN.com ─────────────────
#> ℹ Data updated: 2026-06-12 11:40:35 UTC
#> # A tibble: 0 × 0
# }
```

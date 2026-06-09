# **Get ESPN MLB Season Ranking Detail**

Returns the per-week snapshot index for one ranking source (e.g. AP Top
25). Each row is one weekly snapshot; the `ref` URL resolves to the
actual ranked teams for that (season-type x week) and will be wrapped by
a forthcoming
[`espn_mlb_week_ranking()`](https://billpetti.github.io/baseballr/reference/espn_mlb_week_ranking.md).

## Usage

``` r
espn_mlb_season_ranking(ranking_id, season = most_recent_mlb_season(), ...)
```

## Arguments

- ranking_id:

  ESPN ranking identifier (character or numeric).

- season:

  Season year. Defaults to most recent MLB season.

- ...:

  Additional arguments; currently unused.

## Value

A tibble with one row per weekly snapshot.

|             |           |                                             |
|-------------|-----------|---------------------------------------------|
| col_name    | types     | description                                 |
| league      | character | League slug.                                |
| season      | integer   | Season year.                                |
| ranking_id  | character | ESPN ranking id.                            |
| name        | character | Ranking name (e.g. "AP Top 25").            |
| short_name  | character | Short name (e.g. "AP Poll").                |
| type        | character | Ranking type code (e.g. "ap").              |
| season_type | integer   | Season-type id of this snapshot.            |
| week        | integer   | Week number of this snapshot.               |
| ref         | character | `$ref` URL for the per-week ranking detail. |

## Details

**Get ESPN MLB Season Ranking Detail**

## See also

Other ESPN MLB Functions:
[`espn_mlb`](https://billpetti.github.io/baseballr/reference/espn_mlb.md),
[`espn_mlb_athletes_index()`](https://billpetti.github.io/baseballr/reference/espn_mlb_athletes_index.md),
[`espn_mlb_award()`](https://billpetti.github.io/baseballr/reference/espn_mlb_award.md),
[`espn_mlb_betting()`](https://billpetti.github.io/baseballr/reference/espn_mlb_betting.md),
[`espn_mlb_calendar()`](https://billpetti.github.io/baseballr/reference/espn_mlb_calendar.md),
[`espn_mlb_coach()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach.md),
[`espn_mlb_coach_record()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach_record.md),
[`espn_mlb_coach_season()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach_season.md),
[`espn_mlb_coaches()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coaches.md),
[`espn_mlb_conferences()`](https://billpetti.github.io/baseballr/reference/espn_mlb_conferences.md),
[`espn_mlb_draft()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft.md),
[`espn_mlb_draft_athlete_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_athlete_detail.md),
[`espn_mlb_draft_athletes()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_athletes.md),
[`espn_mlb_draft_pick()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_pick.md),
[`espn_mlb_draft_rounds()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_rounds.md),
[`espn_mlb_draft_status()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_status.md),
[`espn_mlb_franchise()`](https://billpetti.github.io/baseballr/reference/espn_mlb_franchise.md),
[`espn_mlb_franchises()`](https://billpetti.github.io/baseballr/reference/espn_mlb_franchises.md),
[`espn_mlb_freeagents()`](https://billpetti.github.io/baseballr/reference/espn_mlb_freeagents.md),
[`espn_mlb_futures()`](https://billpetti.github.io/baseballr/reference/espn_mlb_futures.md),
[`espn_mlb_game_all()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_all.md),
[`espn_mlb_game_broadcasts()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_broadcasts.md),
[`espn_mlb_game_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_endpoints.md),
[`espn_mlb_game_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_info.md),
[`espn_mlb_game_odds()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_odds.md),
[`espn_mlb_game_official_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_official_detail.md),
[`espn_mlb_game_officials()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_officials.md),
[`espn_mlb_game_play()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_play.md),
[`espn_mlb_game_play_personnel()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_play_personnel.md),
[`espn_mlb_game_player_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_player_box.md),
[`espn_mlb_game_powerindex()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_powerindex.md),
[`espn_mlb_game_predictor()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_predictor.md),
[`espn_mlb_game_probabilities()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_probabilities.md),
[`espn_mlb_game_probables()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_probables.md),
[`espn_mlb_game_propbets()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_propbets.md),
[`espn_mlb_game_rosters()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_rosters.md),
[`espn_mlb_game_situation()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_situation.md),
[`espn_mlb_game_team_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_leaders.md),
[`espn_mlb_game_team_linescores()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_linescores.md),
[`espn_mlb_game_team_records()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_records.md),
[`espn_mlb_game_team_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_roster.md),
[`espn_mlb_game_team_roster_entry()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_roster_entry.md),
[`espn_mlb_game_team_score()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_score.md),
[`espn_mlb_game_team_statistics()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_statistics.md),
[`espn_mlb_injuries()`](https://billpetti.github.io/baseballr/reference/espn_mlb_injuries.md),
[`espn_mlb_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_leaders.md),
[`espn_mlb_news()`](https://billpetti.github.io/baseballr/reference/espn_mlb_news.md),
[`espn_mlb_pbp()`](https://billpetti.github.io/baseballr/reference/espn_mlb_pbp.md),
[`espn_mlb_player_awards()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_awards.md),
[`espn_mlb_player_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_box.md),
[`espn_mlb_player_career_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_career_stats.md),
[`espn_mlb_player_contract()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_contract.md),
[`espn_mlb_player_contracts()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_contracts.md),
[`espn_mlb_player_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_endpoints.md),
[`espn_mlb_player_eventlog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_eventlog.md),
[`espn_mlb_player_eventlog_v2()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_eventlog_v2.md),
[`espn_mlb_player_gamelog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_gamelog.md),
[`espn_mlb_player_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_info.md),
[`espn_mlb_player_overview()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_overview.md),
[`espn_mlb_player_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_seasons.md),
[`espn_mlb_player_splits()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_splits.md),
[`espn_mlb_player_statisticslog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_statisticslog.md),
[`espn_mlb_player_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_stats.md),
[`espn_mlb_player_stats_v3()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_stats_v3.md),
[`espn_mlb_position()`](https://billpetti.github.io/baseballr/reference/espn_mlb_position.md),
[`espn_mlb_positions()`](https://billpetti.github.io/baseballr/reference/espn_mlb_positions.md),
[`espn_mlb_powerindex()`](https://billpetti.github.io/baseballr/reference/espn_mlb_powerindex.md),
[`espn_mlb_scoreboard()`](https://billpetti.github.io/baseballr/reference/espn_mlb_scoreboard.md),
[`espn_mlb_season_awards()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_awards.md),
[`espn_mlb_season_draft()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_draft.md),
[`espn_mlb_season_group()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group.md),
[`espn_mlb_season_group_children()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group_children.md),
[`espn_mlb_season_group_teams()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group_teams.md),
[`espn_mlb_season_groups()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_groups.md),
[`espn_mlb_season_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_info.md),
[`espn_mlb_season_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_leaders.md),
[`espn_mlb_season_rankings()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_rankings.md),
[`espn_mlb_season_type()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_type.md),
[`espn_mlb_season_types()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_types.md),
[`espn_mlb_season_week()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_week.md),
[`espn_mlb_season_weeks()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_weeks.md),
[`espn_mlb_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_seasons.md),
[`espn_mlb_standings()`](https://billpetti.github.io/baseballr/reference/espn_mlb_standings.md),
[`espn_mlb_team()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team.md),
[`espn_mlb_team_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_box.md),
[`espn_mlb_team_current_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_current_roster.md),
[`espn_mlb_team_depthchart()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_depthchart.md),
[`espn_mlb_team_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_endpoints.md),
[`espn_mlb_team_injuries()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_injuries.md),
[`espn_mlb_team_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_leaders.md),
[`espn_mlb_team_news()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_news.md),
[`espn_mlb_team_odds_records()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_odds_records.md),
[`espn_mlb_team_record()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_record.md),
[`espn_mlb_team_record_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_record_detail.md),
[`espn_mlb_team_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_roster.md),
[`espn_mlb_team_schedule()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_schedule.md),
[`espn_mlb_team_season_profile()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_profile.md),
[`espn_mlb_team_season_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_roster.md),
[`espn_mlb_team_season_statistics()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_statistics.md),
[`espn_mlb_team_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_stats.md),
[`espn_mlb_teams()`](https://billpetti.github.io/baseballr/reference/espn_mlb_teams.md),
[`espn_mlb_tournament()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament.md),
[`espn_mlb_tournament_season()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament_season.md),
[`espn_mlb_tournament_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament_seasons.md),
[`espn_mlb_tournaments()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournaments.md),
[`espn_mlb_transactions()`](https://billpetti.github.io/baseballr/reference/espn_mlb_transactions.md),
[`espn_mlb_venues()`](https://billpetti.github.io/baseballr/reference/espn_mlb_venues.md),
[`espn_mlb_week_ranking()`](https://billpetti.github.io/baseballr/reference/espn_mlb_week_ranking.md),
[`espn_mlb_week_rankings()`](https://billpetti.github.io/baseballr/reference/espn_mlb_week_rankings.md),
[`espn_mlb_wp()`](https://billpetti.github.io/baseballr/reference/espn_mlb_wp.md)

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  espn_mlb_season_ranking(ranking_id = 1, season = 2025)
#> ── ESPN MLB Season Ranking Snapshots Index ────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-09 20:42:29 UTC
#> # A tibble: 0 × 9
#> # ℹ 9 variables: league <chr>, season <int>, ranking_id <chr>,
#> #   name <chr>, short_name <chr>, type <chr>, season_type <int>,
#> #   week <int>, ref <chr>
# }
```

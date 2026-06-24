# **Get ESPN College Baseball Team Box**

**Get ESPN College Baseball team box scores**

## Usage

``` r
espn_college_baseball_team_box(game_id)
```

## Arguments

- game_id:

  Game ID

## Value

A team boxscore data frame

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_id | integer | Unique ESPN game/event identifier. |
| season | integer | Season (4-digit year). |
| season_type | integer | ESPN season type (1=pre, 2=regular, 3=postseason, 4=off-season). |
| game_date | Date | Game date (YYYY-MM-DD). |
| game_date_time | POSIXct | Game start date/time (US/Eastern). |
| team_id | integer | Unique ESPN team identifier. |
| team_uid | character | ESPN universal team identifier (UID). |
| team_slug | character | URL-safe team identifier. |
| team_location | character | Team city / location. |
| team_name | character | Team nickname (e.g. 'Yankees'). |
| team_abbreviation | character | Short team abbreviation (e.g. 'NYY'). |
| team_display_name | character | Full team display name (e.g. 'New York Yankees'). |
| team_short_display_name | character | Short team display name. |
| team_color | character | Team primary color (hex, no leading '#'). |
| team_alternate_color | character | Team alternate color (hex). |
| team_logo | character | Team logo image URL. |
| batting_games_played | character | Team batting: batting games played. |
| batting_team_games_played | character | Team batting: batting team games played. |
| batting_hit_by_pitch | character | Team batting: batting hit by pitch. |
| batting_ground_balls | character | Team batting: batting ground balls. |
| batting_strikeouts | character | Team batting: batting strikeouts. |
| batting_rb_is | character | Team batting: batting rb is. |
| batting_sac_hits | character | Team batting: batting sac hits. |
| batting_hits | character | Team batting: batting hits. |
| batting_stolen_bases | character | Team batting: batting stolen bases. |
| batting_walks | character | Team batting: batting walks. |
| batting_catcher_interference | character | Team batting: batting catcher interference. |
| batting_runs | character | Team batting: batting runs. |
| batting_gid_ps | character | Team batting: batting gid ps. |
| batting_sac_flies | character | Team batting: batting sac flies. |
| batting_at_bats | character | Team batting: batting at bats. |
| batting_home_runs | character | Team batting: batting home runs. |
| batting_grand_slam_home_runs | character | Team batting: batting grand slam home runs. |
| batting_runners_left_on_base | character | Team batting: batting runners left on base. |
| batting_triples | character | Team batting: batting triples. |
| batting_game_winning_rb_is | character | Team batting: batting game winning rb is. |
| batting_intentional_walks | character | Team batting: batting intentional walks. |
| batting_doubles | character | Team batting: batting doubles. |
| batting_fly_balls | character | Team batting: batting fly balls. |
| batting_caught_stealing | character | Team batting: batting caught stealing. |
| batting_pitches | character | Team batting: batting pitches. |
| batting_games_started | character | Team batting: batting games started. |
| batting_pinch_at_bats | character | Team batting: batting pinch at bats. |
| batting_pinch_hits | character | Team batting: batting pinch hits. |
| batting_player_rating | character | Team batting: batting player rating. |
| batting_is_qualified | character | Team batting: batting is qualified. |
| batting_is_qualified_steals | character | Team batting: batting is qualified steals. |
| batting_total_bases | character | Team batting: batting total bases. |
| batting_plate_appearances | character | Team batting: batting plate appearances. |
| batting_projected_home_runs | character | Team batting: batting projected home runs. |
| batting_extra_base_hits | character | Team batting: batting extra base hits. |
| batting_runs_created | character | Team batting: batting runs created. |
| batting_avg | character | Team batting: batting average. |
| batting_pinch_avg | character | Team batting: batting pinch avg. |
| batting_slug_avg | character | Team batting: batting slug avg. |
| batting_secondary_avg | character | Team batting: batting secondary avg. |
| batting_on_base_pct | character | Team batting: batting on base pct. |
| batting_ops | character | Team batting: batting ops. |
| batting_ground_to_fly_ratio | character | Team batting: batting ground to fly ratio. |
| batting_runs_created_per27outs | character | Team batting: batting runs created per27outs. |
| batting_batter_rating | character | Team batting: batting batter rating. |
| batting_at_bats_per_home_run | character | Team batting: batting at bats per home run. |
| batting_stolen_base_pct | character | Team batting: batting stolen base pct. |
| batting_pitches_per_plate_appearance | character | Team batting: batting pitches per plate appearance. |
| batting_isolated_power | character | Team batting: batting isolated power. |
| batting_walk_to_strikeout_ratio | character | Team batting: batting walk to strikeout ratio. |
| batting_walks_per_plate_appearance | character | Team batting: batting walks per plate appearance. |
| batting_secondary_avg_minus_ba | character | Team batting: batting secondary avg minus ba. |
| batting_runs_produced | character | Team batting: batting runs produced. |
| batting_runs_ratio | character | Team batting: batting runs ratio. |
| ...and 128 further ESPN stat columns (full batting / pitching / fielding stat set). |  |  |

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
try(espn_college_baseball_team_box(game_id = "401778093"))
#> ── ESPN MLB Team Box Information from ESPN.com ────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-24 02:04:31 UTC
#> # A tibble: 2 × 96
#>     game_id season season_type game_date  game_date_time      team_id
#>       <int>  <int>       <int> <date>     <dttm>                <int>
#> 1 401778093   2025           5 2025-06-15 2025-06-15 19:00:00     146
#> 2 401778093   2025           5 2025-06-15 2025-06-15 19:00:00     113
#> # ℹ 90 more variables: team_uid <chr>, team_slug <chr>,
#> #   team_location <chr>, team_name <chr>, team_abbreviation <chr>,
#> #   team_display_name <chr>, team_short_display_name <chr>,
#> #   team_color <chr>, team_alternate_color <chr>, team_logo <chr>,
#> #   batting_hit_by_pitch <chr>, batting_strikeouts <chr>,
#> #   batting_rb_is <chr>, batting_sac_hits <chr>, batting_hits <chr>,
#> #   batting_stolen_bases <chr>, batting_walks <chr>, …
# }
```

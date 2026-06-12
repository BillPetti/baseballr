# **Get ESPN College Baseball Athlete Info**

**Get ESPN College Baseball Athlete Info**

**Get ESPN College Baseball Athlete Info**

## Usage

``` r
espn_college_baseball_player_info(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Additional arguments; currently unused but retained for forward
  compatibility. Proxy configuration should use
  `options(baseballr.proxy = ...)` – see
  [`?baseballr`](https://billpetti.github.io/baseballr/reference/baseballr-package.md)
  for details.

## Value

A named list of data frames: `Bio`, `Team`, `Position`, `Status`,
`College`, `Draft`.

**Bio**

|               |           |                                   |
|---------------|-----------|-----------------------------------|
| col_name      | types     | description                       |
| id            | character | Id.                               |
| full_name     | character | Player's full name.               |
| display_name  | character | Display name.                     |
| jersey        | character | Jersey number worn by the player. |
| age           | character | Player age (in years).            |
| date_of_birth | character | Date of birth (YYYY-MM-DD).       |
| headshot_href | character | Headshot image URL.               |

**Team**

|              |           |                     |
|--------------|-----------|---------------------|
| col_name     | types     | description         |
| id           | character | Id.                 |
| abbreviation | character | Short abbreviation. |
| display_name | character | Display name.       |

**Position**

|              |           |                     |
|--------------|-----------|---------------------|
| col_name     | types     | description         |
| id           | character | Id.                 |
| name         | character | Display name.       |
| abbreviation | character | Short abbreviation. |

**Status**

|          |           |                         |
|----------|-----------|-------------------------|
| col_name | types     | description             |
| id       | character | Id.                     |
| name     | character | Display name.           |
| type     | character | Record type / category. |

**College**

|          |           |               |
|----------|-----------|---------------|
| col_name | types     | description   |
| id       | character | Id.           |
| name     | character | Display name. |
| mascot   | character | Team mascot.  |

**Draft**

|           |           |                             |
|-----------|-----------|-----------------------------|
| col_name  | types     | description                 |
| year      | character | 4-digit year.               |
| round     | character | Tournament / playoff round. |
| selection | character |                             |

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
  try(espn_college_baseball_player_info(athlete_id = "3730"))
#> $Bio
#> ── ESPN COLLEGE-BASEBALL Athlete Bio from ESPN.com ── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 13:44:40 UTC
#> # A tibble: 1 × 11
#>   id    uid       first_name last_name full_name display_name short_name
#>   <chr> <chr>     <chr>      <chr>     <chr>     <chr>        <chr>     
#> 1 3730  s:1~l:14… J          Aboites   J Aboites J Aboites    J. Aboites
#> # ℹ 4 more variables: active <lgl>, birth_city <chr>,
#> #   birth_state <chr>, birth_country <chr>
#> 
#> $Team
#> ── ESPN COLLEGE-BASEBALL Athlete Team from ESPN.com ────────────────────
#> ℹ Data updated: 2026-06-12 13:44:40 UTC
#> # A tibble: 1 × 1
#>   x_ref                                                                 
#>   <chr>                                                                 
#> 1 http://sports.core.api.espn.com/v2/sports/baseball/leagues/college-ba…
#> 
#> $Position
#> ── ESPN COLLEGE-BASEBALL Athlete Position from ESPN.com ────────────────
#> ℹ Data updated: 2026-06-12 13:44:40 UTC
#> # A tibble: 1 × 5
#>   id    name                 display_name         abbreviation leaf 
#>   <chr> <chr>                <chr>                <chr>        <lgl>
#> 1 99    Unspecified Position Unspecified Position UN           TRUE 
#> 
#> $Status
#> ── ESPN COLLEGE-BASEBALL Athlete Status from ESPN.com ──────────────────
#> ℹ Data updated: 2026-06-12 13:44:40 UTC
#> # A tibble: 0 × 0
#> 
#> $College
#> ── ESPN COLLEGE-BASEBALL Athlete College from ESPN.com ─────────────────
#> ℹ Data updated: 2026-06-12 13:44:40 UTC
#> # A tibble: 0 × 0
#> 
#> $Draft
#> ── ESPN COLLEGE-BASEBALL Athlete Draft from ESPN.com ───────────────────
#> ℹ Data updated: 2026-06-12 13:44:40 UTC
#> # A tibble: 0 × 0
#> 
# }
```

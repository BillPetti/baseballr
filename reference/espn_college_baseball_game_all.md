# **Get ESPN College Baseball Game All**

**Get ESPN College Baseball game data (Pbp, Team and Player Box)**

## Usage

``` r
espn_college_baseball_game_all(game_id)
```

## Arguments

- game_id:

  Game ID

## Value

A named list of data frames: Plays, Team, Player

[`espn_mlb_game_all()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_all.md)
bundles three tibbles returned in one call. Each component is identical
to the matching standalone function – see those for the full column
documentation:

- **Plays** – play-by-play, identical to
  [`espn_mlb_pbp()`](https://billpetti.github.io/baseballr/reference/espn_mlb_pbp.md).

- **Team** – team box score, identical to
  [`espn_mlb_team_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_box.md).

- **Player** – player box score, identical to
  [`espn_mlb_player_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_box.md).

## See also

Other ESPN College Baseball Functions:
[`espn_college_baseball_athletes_index()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_athletes_index.md),
[`espn_college_baseball_calendar()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_calendar.md),
[`espn_college_baseball_coach()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_coach.md),
[`espn_college_baseball_coach_record()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_coach_record.md),
[`espn_college_baseball_coach_season()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_coach_season.md),
[`espn_college_baseball_coaches()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_coaches.md),
[`espn_college_baseball_conferences()`](https://billpetti.github.io/baseballr/reference/espn_college_baseball_conferences.md),
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
try(espn_college_baseball_game_all(game_id = "401778093"))
#> New names:
#> • `` -> `...45`
#> • `` -> `...46`
#> $Plays
#> ── ESPN MLB Play-by-Play Information from ESPN.com ── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:54:55 UTC
#> # A tibble: 581 × 72
#>    id           sequence_number text  away_score home_score scoring_play
#>    <chr>        <chr>           <chr>      <int>      <int> <lgl>       
#>  1 40177809300… 1               "Top…          0          0 FALSE       
#>  2 40177809300… 1               "E. …          0          0 FALSE       
#>  3 40177809300… 2               "Pit…          0          0 FALSE       
#>  4 40177809300… 3               "Pit…          0          0 FALSE       
#>  5 40177809300… 4               "Pit…          0          0 FALSE       
#>  6 40177809300… 5               "C. …          0          0 FALSE       
#>  7 40177809300… 6                NA            0          0 FALSE       
#>  8 40177809300… 1               "E. …          0          0 FALSE       
#>  9 40177809300… 2               "C. …          1          0 FALSE       
#> 10 40177809300… 3               "C. …          1          0 FALSE       
#> # ℹ 571 more rows
#> # ℹ 66 more variables: score_value <int>, wallclock <chr>,
#> #   at_bat_id <chr>, summary_type <chr>, outs <int>, type <list<chr>>,
#> #   bat_order <int>, at_bat_pitch_number <int>, alternative_play <chr>,
#> #   type_id <int>, type_text <chr>, type_abbreviation <chr>,
#> #   period_type <chr>, period_number <int>, period_display_value <chr>,
#> #   team_id <int>, pitch_count_balls <int>, …
#> 
#> $Team
#> ── ESPN MLB Team Box Information from ESPN.com ────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:54:55 UTC
#> # A tibble: 2 × 95
#>     game_id season season_type game_date  game_date_time      team_id
#>       <int>  <int>       <int> <date>     <dttm>                <int>
#> 1 401778093   2025           5 2025-06-15 2025-06-15 19:00:00     146
#> 2 401778093   2025           5 2025-06-15 2025-06-15 19:00:00     113
#> # ℹ 89 more variables: team_uid <chr>, team_slug <chr>,
#> #   team_location <chr>, team_name <chr>, team_abbreviation <chr>,
#> #   team_display_name <chr>, team_short_display_name <chr>,
#> #   team_color <chr>, team_alternate_color <chr>, team_logo <chr>,
#> #   batting_hit_by_pitch <chr>, batting_strikeouts <chr>,
#> #   batting_rb_is <chr>, batting_sac_hits <chr>, batting_hits <chr>,
#> #   batting_stolen_bases <chr>, batting_walks <chr>, …
#> 
#> $Player
#> ── ESPN MLB Player Box Information from ESPN.com ──── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:54:55 UTC
#> # A tibble: 25 × 35
#>     game_id season season_type game_date  game_date_time      stat_group
#>       <int>  <int>       <int> <date>     <dttm>              <chr>     
#>  1   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#>  2   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#>  3   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#>  4   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#>  5   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#>  6   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#>  7   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#>  8   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#>  9   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 batting   
#> 10   4.02e8   2025           5 2025-06-15 2025-06-15 19:00:00 pitching  
#> # ℹ 15 more rows
#> # ℹ 29 more variables: team_id <int>, team_name <chr>,
#> #   team_abbreviation <chr>, team_display_name <chr>, athlete_id <int>,
#> #   athlete_display_name <chr>, athlete_short_name <chr>,
#> #   athlete_position_name <chr>, athlete_position_abbreviation <chr>,
#> #   starter <lgl>, bat_order <int>, active <lgl>, h_ab <chr>, ab <chr>,
#> #   r <chr>, h <chr>, rbi <chr>, hr <chr>, bb <chr>, k <chr>, …
#> 
# }
```

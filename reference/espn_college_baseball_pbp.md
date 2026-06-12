# **Get ESPN College Baseball Play-by-Play**

**Get ESPN College Baseball PBP data**

## Usage

``` r
espn_college_baseball_pbp(game_id)
```

## Arguments

- game_id:

  Game ID

## Value

A play-by-play data frame.

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | character | Play / record identifier. |
| sequence_number | character | Play sequence number within the game. |
| text | character | Text description of the play. |
| away_score | integer | Away team run total after the play. |
| home_score | integer | Home team run total after the play. |
| scoring_play | logical | TRUE if the play scored a run. |
| score_value | integer | Runs scored on the play. |
| wallclock | character | Wall-clock timestamp of the play (ISO 8601). |
| at_bat_id | character | Identifier of the at-bat the play belongs to. |
| summary_type | character | Play summary type. |
| outs | integer | Outs in the inning after the play. |
| type | character | Type. |
| bat_order | integer | Spot in the batting order (1-9; NA if not applicable). |
| at_bat_pitch_number | integer | Pitch number within the at-bat. |
| pitch_velocity | integer | Pitch velocity (mph). |
| trajectory | character | Batted-ball trajectory. |
| alternative_play | character | Alternative play. |
| type_id | integer | Type id. |
| type_text | character | Type text. |
| type_type | character | Type type. |
| type_alternative_text | character | Type alternative text. |
| type_abbreviation | character | Type abbreviation. |
| period_type | character | Period type ('inning'). |
| period_number | integer | Inning number. |
| period_display_value | character | Inning display value (e.g. 'Top 1st'). |
| team_id | integer | Unique ESPN team identifier. |
| pitch_count_balls | integer | Balls in the count when the pitch was thrown. |
| pitch_count_strikes | integer | Strikes in the count when the pitch was thrown. |
| result_count_balls | integer | Balls in the count after the pitch. |
| result_count_strikes | integer | Strikes in the count after the pitch. |
| bats_type | character | Bats type. |
| bats_abbreviation | character | Bats abbreviation. |
| bats_display_value | character | Bats display value. |
| pitch_coordinate_x | integer | Pitch location x-coordinate. |
| pitch_coordinate_y | integer | Pitch location y-coordinate. |
| pitch_type_id | character | Pitch type identifier. |
| pitch_type_text | character | Pitch type description (e.g. 'Four-seam FB'). |
| pitch_type_abbreviation | character | Pitch type abbreviation. |
| hit_coordinate_x | integer | Batted-ball location x-coordinate. |
| hit_coordinate_y | integer | Batted-ball location y-coordinate. |
| alternative_type_id | character | Alternative type id. |
| alternative_type_text | character | Alternative type text. |
| alternative_type_abbreviation | character | Alternative type abbreviation. |
| alternative_type_alternative_text | character | Alternative type alternative text. |
| alternative_type_type | character | Alternative type type. |
| on_first_athlete_id | character | Athlete id of the runner on first base. |
| on_second_athlete_id | character | Athlete id of the runner on second base. |
| play_id | character | Play id. |
| athlete_id_1 | integer | Primary participating athlete id (batter). |
| athlete_id_2 | integer | Second participating athlete id (pitcher). |
| athlete_id_3 | integer | Third participating athlete id (fielder). |
| home_team_id | integer | Home team id. |
| home_team_mascot | character | Home team mascot. |
| home_team_name | character | Home team name. |
| home_team_abbrev | character | Home team abbrev. |
| home_team_logo | character | Home team logo. |
| home_team_logo_dark | character | Home team logo dark. |
| home_team_full_name | character | Home team full name. |
| home_team_color | character | Home team color. |
| home_team_alternate_color | character | Home team alternate color. |
| home_team_score | integer | Home team score. |
| home_team_winner | logical | Home team winner. |
| home_team_record | character | Home team record. |
| away_team_id | integer | Away team id. |
| away_team_mascot | character | Away team mascot. |
| away_team_name | character | Away team name. |
| away_team_abbrev | character | Away team abbrev. |
| away_team_logo | character | Away team logo. |
| away_team_logo_dark | character | Away team logo dark. |
| away_team_full_name | character | Away team full name. |
| away_team_color | character | Away team color. |
| away_team_alternate_color | character | Away team alternate color. |
| away_team_score | integer | Away team score. |
| away_team_winner | logical | Away team winner. |
| away_team_record | character | Away team record. |
| game_id | integer | Unique ESPN game/event identifier. |
| season | integer | Season (4-digit year). |
| season_type | integer | ESPN season type (1=pre, 2=regular, 3=postseason, 4=off-season). |
| game_date | Date | Game date (YYYY-MM-DD). |
| game_date_time | POSIXct | Game start date/time (US/Eastern). |

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
try(espn_college_baseball_pbp(game_id = "401778093"))
#> New names:
#> • `` -> `...45`
#> • `` -> `...46`
#> ── ESPN MLB Play-by-Play Information from ESPN.com ── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:40:33 UTC
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
# }
```

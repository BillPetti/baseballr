# **Find MLB Player Game Stats - Current Game**

**Find MLB Player Game Stats - Current Game**

## Usage

``` r
mlb_player_game_stats_current(person_id = NULL)
```

## Arguments

- person_id:

  MLBAMIDs for player of interest.

## Value

Returns a tibble with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| type | character | Stat type/category (e.g. "gameLog"). |
| group | character | Stat group (e.g. "hitting", "pitching", "fielding"). |
| stat_assists | integer | Assists. |
| stat_put_outs | integer | Putouts. |
| stat_errors | integer | Errors. |
| stat_chances | integer | Total chances (fielding). |
| stat_fielding | character | Fielding percentage. |
| stat_caught_stealing | integer | Runners caught stealing. |
| stat_passed_ball | integer | Passed balls. |
| stat_stolen_bases | integer | Stolen bases allowed (catcher/fielding). |
| stat_stolen_base_percentage | character | Stolen-base percentage. |
| stat_pickoffs | integer | Pickoffs. |
| stat_games_played | integer | Games played. |
| stat_games_started | integer | Games started. |
| stat_fly_outs | integer | Fly outs. |
| stat_ground_outs | integer | Ground outs. |
| stat_air_outs | integer | Air outs (fly + line outs). |
| stat_runs | integer | Runs. |
| stat_doubles | integer | Doubles. |
| stat_triples | integer | Triples. |
| stat_home_runs | integer | Home runs. |
| stat_strike_outs | integer | Strikeouts. |
| stat_base_on_balls | integer | Walks (bases on balls). |
| stat_intentional_walks | integer | Intentional walks. |
| stat_hits | integer | Hits. |
| stat_hit_by_pitch | integer | Hit by pitch. |
| stat_at_bats | integer | At-bats. |
| stat_number_of_pitches | integer | Number of pitches. |
| stat_innings_pitched | character | Innings pitched. |
| stat_wins | integer | Wins. |
| stat_losses | integer | Losses. |
| stat_saves | integer | Saves. |
| stat_save_opportunities | integer | Save opportunities. |
| stat_holds | integer | Holds. |
| stat_blown_saves | integer | Blown saves. |
| stat_earned_runs | integer | Earned runs. |
| stat_batters_faced | integer | Batters faced. |
| stat_outs | integer | Outs recorded. |
| stat_games_pitched | integer | Games pitched. |
| stat_complete_games | integer | Complete games. |
| stat_shutouts | integer | Shutouts. |
| stat_pitches_thrown | integer | Pitches thrown. |
| stat_balls | integer | Balls thrown. |
| stat_strikes | integer | Strikes thrown. |
| stat_strike_percentage | character | Strike percentage. |
| stat_hit_batsmen | integer | Hit batsmen. |
| stat_balks | integer | Balks. |
| stat_wild_pitches | integer | Wild pitches. |
| stat_rbi | integer | Runs batted in. |
| stat_games_finished | integer | Games finished. |
| stat_runs_scored_per9 | character | Runs scored per nine innings. |
| stat_home_runs_per9 | character | Home runs allowed per nine innings. |
| stat_inherited_runners | integer | Inherited runners. |
| stat_inherited_runners_scored | integer | Inherited runners who scored. |
| stat_catchers_interference | integer | Catcher's interference. |
| stat_sac_bunts | integer | Sacrifice bunts. |
| stat_sac_flies | integer | Sacrifice flies. |
| stat_ground_into_double_play | integer | Grounded into double plays. |
| stat_ground_into_triple_play | integer | Grounded into triple plays. |
| stat_plate_appearances | integer | Plate appearances. |
| stat_total_bases | integer | Total bases. |
| stat_left_on_base | integer | Runners left on base. |
| stat_at_bats_per_home_run | character | At-bats per home run. |
| game_type | character | Game type (e.g. regular season, postseason). |
| num_teams | integer | Number of teams represented in the split. |
| stat_avg | character | Batting average. |
| stat_obp | character | On-base percentage. |
| stat_slg | character | Slugging percentage. |
| stat_ops | character | On-base plus slugging. |
| stat_outs_pitched | integer | Outs pitched. |
| stat_whip | character | Walks plus hits per inning pitched. |
| stat_ground_outs_to_airouts | character | Ground-out to air-out ratio. |
| stat_pitches_per_inning | character | Pitches per inning. |
| stat_strikeout_walk_ratio | character | Strikeout-to-walk ratio. |
| stat_strikeouts_per9inn | character | Strikeouts per nine innings. |
| stat_walks_per9inn | character | Walks per nine innings. |
| stat_hits_per9inn | character | Hits per nine innings. |
| team_id | integer | Team id (MLBAM). |
| team_name | character | Team name. |
| team_link | character | MLB Stats API relative link for the team. |
| opponent_id | integer | Opponent team id. |
| opponent_name | character | Opponent team name. |
| opponent_link | character | MLB Stats API relative link for the opponent. |
| pitcher_id | integer | Pitcher MLBAMID. |
| pitcher_full_name | character | Pitcher full name. |
| pitcher_link | character | MLB Stats API relative link for the pitcher. |
| pitcher_first_name | character | Pitcher first name. |
| pitcher_last_name | character | Pitcher last name. |
| batter_id | integer | Batter MLBAMID. |
| batter_full_name | character | Batter full name. |
| batter_link | character | MLB Stats API relative link for the batter. |
| batter_first_name | character | Batter first name. |
| batter_last_name | character | Batter last name. |
| total_splits | integer | Number of stat splits returned. |
| type_display_name | character | Display name of the stat type. |
| group_display_name | character | Display name of the stat group. |
| player_id | numeric | Player MLBAMID. |
| game_pk | numeric | Game primary key (MLBAM game id). |

## Examples

``` r
# \donttest{
  try(mlb_player_game_stats_current(person_id = 660271))
#> ── MLB Player Game Stats - Current Game data from MLB.com ──────────────
#> ℹ Data updated: 2026-06-12 11:57:14 UTC
#> # A tibble: 0 × 0
# }
```

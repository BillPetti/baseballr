# **Find MLB Player Game Stats**

**Find MLB Player Game Stats**

## Usage

``` r
mlb_player_game_stats(person_id = NULL, game_pk = NULL)
```

## Arguments

- person_id:

  MLBAMIDs for player of interest.

- game_pk:

  The game_pk to return game_log statistics for a specific player in a
  specific game and to complete the call.

## Value

Returns a tibble with one row per stat group (fielding, hitting,
pitching) for the player in the game, with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| type | character | Stat type returned (e.g. 'gameLog'). |
| group | character | Stat group: fielding, hitting, or pitching. |
| caught_stealing | integer | Runners caught stealing. |
| stolen_bases | integer | Stolen bases. |
| stolen_base_percentage | character | Stolen base success rate. |
| caught_stealing_percentage | character | Caught-stealing rate (catcher fielding). |
| assists | integer | Fielding assists. |
| put_outs | integer | Fielding putouts. |
| errors | integer | Fielding errors. |
| chances | integer | Total fielding chances. |
| fielding | character | Fielding percentage. |
| passed_ball | integer | Passed balls (catcher). |
| pickoffs | integer | Pickoffs. |
| summary | character | Text summary line of the player's game. |
| games_played | integer | Games played. |
| games_started | integer | Games started. |
| fly_outs | integer | Fly-ball outs. |
| ground_outs | integer | Ground-ball outs. |
| air_outs | integer | Air outs (fly outs + line outs + pop outs). |
| runs | integer | Runs. |
| doubles | integer | Doubles. |
| triples | integer | Triples. |
| home_runs | integer | Home runs. |
| strike_outs | integer | Strikeouts. |
| base_on_balls | integer | Walks. |
| intentional_walks | integer | Intentional walks. |
| hits | integer | Hits. |
| hit_by_pitch | integer | Hit by pitch. |
| at_bats | integer | At-bats. |
| number_of_pitches | integer | Number of pitches seen or thrown. |
| innings_pitched | character | Innings pitched. |
| wins | integer | Pitcher wins. |
| losses | integer | Pitcher losses. |
| saves | integer | Saves. |
| save_opportunities | integer | Save opportunities. |
| holds | integer | Holds. |
| blown_saves | integer | Blown saves. |
| earned_runs | integer | Earned runs allowed. |
| batters_faced | integer | Batters faced. |
| outs | integer | Outs recorded. |
| games_pitched | integer | Games pitched. |
| complete_games | integer | Complete games. |
| shutouts | integer | Shutouts. |
| pitches_thrown | integer | Pitches thrown. |
| balls | integer | Balls thrown. |
| strikes | integer | Strikes thrown. |
| strike_percentage | character | Share of pitches that were strikes. |
| hit_batsmen | integer | Batters hit by pitch (pitcher). |
| balks | integer | Balks. |
| wild_pitches | integer | Wild pitches. |
| rbi | integer | Runs batted in. |
| games_finished | integer | Games finished. |
| runs_scored_per9 | character | Runs scored per nine innings. |
| home_runs_per9 | character | Home runs allowed per nine innings. |
| inherited_runners | integer | Inherited runners. |
| inherited_runners_scored | integer | Inherited runners who scored. |
| catchers_interference | integer | Catcher's interference. |
| sac_bunts | integer | Sacrifice bunts. |
| sac_flies | integer | Sacrifice flies. |
| ground_into_double_play | integer | Grounded into double plays. |
| ground_into_triple_play | integer | Grounded into triple plays. |
| plate_appearances | integer | Plate appearances. |
| total_bases | integer | Total bases. |
| left_on_base | integer | Runners left on base. |
| at_bats_per_home_run | character | At-bats per home run. |
| total_splits | integer | Number of stat splits returned. |
| type_display_name | character | Display name of the stat type. |
| group_display_name | character | Display name of the stat group. |
| player_id | numeric | MLBAM player ID supplied in the request. |
| game_pk | numeric | Game ID supplied in the request. |

## Examples

``` r
# \donttest{
 try(mlb_player_game_stats(person_id = 605151, game_pk = 531368))
#> ── MLB Player Game Stats data from MLB.com ────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 14:09:45 UTC
#> # A tibble: 3 × 70
#>   type    group    caught_stealing stolen_bases stolen_base_percentage
#>   <chr>   <chr>              <int>        <int> <chr>                 
#> 1 gameLog fielding               0            0 .---                  
#> 2 gameLog pitching               0            0 .---                  
#> 3 gameLog hitting                0            0 .---                  
#> # ℹ 65 more variables: caught_stealing_percentage <chr>, assists <int>,
#> #   put_outs <int>, errors <int>, chances <int>, fielding <chr>,
#> #   passed_ball <int>, pickoffs <int>, summary <chr>,
#> #   games_played <int>, games_started <int>, fly_outs <int>,
#> #   ground_outs <int>, air_outs <int>, runs <int>, doubles <int>,
#> #   triples <int>, home_runs <int>, strike_outs <int>,
#> #   base_on_balls <int>, intentional_walks <int>, hits <int>, …
# }
```

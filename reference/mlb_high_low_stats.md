# **Acquire high/low stats for Major and Minor Leagues**

**Acquire high/low stats for Major and Minor Leagues**

## Usage

``` r
mlb_high_low_stats(
  org_type,
  season,
  sort_stat,
  team_ids = NULL,
  league_ids = NULL,
  sport_ids = NULL,
  game_type = NULL,
  stat_group = NULL,
  limit = NULL
)
```

## Arguments

- org_type:

  The organization type for return information (*Required*). Valid
  values include:

  - player

  - team

  - division

  - league

  - sport

- season:

  The season for which you want to return information (*Required*).

- sort_stat:

  The stat to sort the return (*Required*). Valid values can be found
  from 'stat_lookup_param' below

  |  |  |  |  |  |  |  |
  |----|----|----|----|----|----|----|
  | stat_name | stat_lookup_param | is_counting | stat_label | stat_groups | org_types | high_low_types |
  | at_bats | atBats | TRUE | At bats | hitting , pitching | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | total_plate_appearances | plateAppearances | TRUE | Total plate appearances | hitting | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | runs | runs | TRUE | Runs | hitting | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | runs_batted_in | rbi | TRUE | Runs batted in | hitting | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | home_team_runs | runs | TRUE | Home team runs | hitting | TEAM | GAME |
  | away_team_runs | runs | TRUE | Away team runs | hitting | TEAM | GAME |
  | hits | hits | TRUE | Hits | hitting | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | hits_risp | hitsRisp | TRUE | Hits risp | hitting | PLAYER, TEAM | PLAYER, TEAM |
  | home_team_hits | hits | TRUE | Home team hits | hitting | TEAM | GAME |
  | away_team_hits | hits | TRUE | Away team hits | hitting | TEAM | GAME |
  | total_bases | totalBases | TRUE | Total bases | hitting , pitching | PLAYER, TEAM | PLAYER, TEAM |
  | doubles | doubles | TRUE | Doubles | hitting , pitching | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | triples | triples | TRUE | Triples | hitting | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | home_runs | homeRuns | TRUE | Home runs | hitting , pitching | PLAYER, TEAM | PLAYER, TEAM |
  | extra_base_hits | extraBaseHits | TRUE | Extra base hits | hitting | PLAYER, TEAM | PLAYER, TEAM |
  | walks | baseOnBalls | TRUE | Walks | hitting , pitching | PLAYER, TEAM | PLAYER, TEAM |
  | strikeouts | strikeouts | TRUE | Strikeouts | hitting , pitching | PLAYER, TEAM | PLAYER, TEAM |
  | stolen_bases | stolenBases | TRUE | Stolen bases | hitting | PLAYER, TEAM | PLAYER, TEAM |
  | caught_stealing | caughtStealing | TRUE | Caught stealing | hitting , pitching, fielding | PLAYER, TEAM | PLAYER, TEAM |
  | sacrifice_flies | sacFlies | TRUE | Sacrifice flies | hitting | PLAYER, TEAM | PLAYER, TEAM |
  | sacrifice_bunts | sacBunts | TRUE | Sacrifice bunts | hitting | PLAYER, TEAM | PLAYER, TEAM |
  | hit_by_pitches | hitByPitch | TRUE | Hit by pitches | hitting , pitching | PLAYER, TEAM | PLAYER, TEAM |
  | left_on_base | leftOnBase | TRUE | Left on base | hitting | PLAYER, TEAM | PLAYER, TEAM |
  | ground_into_double_plays | groundIntoDoublePlay | TRUE | Ground into double plays | hitting , pitching | PLAYER, TEAM | PLAYER, TEAM |
  | strikes | strikes | TRUE | Strikes | pitching | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | pitches | pitchesThrown | TRUE | Pitches | pitching | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | balks | balks | TRUE | Balks | pitching | PLAYER, TEAM | PLAYER, TEAM , GAME |
  | innings_pitched | inningsPitched | TRUE | Innings pitched | pitching | PLAYER | PLAYER |
  | errors | errors | TRUE | Errors | fielding | TEAM |  |
  | home_team_errors | errors | TRUE | Home team errors | fielding | TEAM |  |
  | away_team_errors | errors | TRUE | Away team errors | fielding | TEAM |  |
  | chances | chances | TRUE | Chances | fielding | TEAM |  |
  | put_outs | putOuts | TRUE | Put outs | fielding | TEAM |  |
  | assists | assists | TRUE | Assists | fielding | TEAM |  |
  | double_plays | doublePlays | TRUE | Double plays | fielding | TEAM |  |
  | attendance | attendance | TRUE | Attendance | game | TEAM | GAME |
  | game_time | gameDuration | TRUE | Game time | game | TEAM | GAME |
  | delay_time | gameDuration | TRUE | Delay time | game | TEAM | GAME |
  | longest | gameDuration | TRUE | Longest | game | TEAM |  |
  | shortest | gameDuration | TRUE | Shortest | game | TEAM |  |
  | inning | innings | TRUE | Inning | game | TEAM | GAME |
  | win_streak | winStreak | TRUE | Win streak | streak | TEAM |  |
  | loss_streak | lossStreak | TRUE | Loss streak | streak | TEAM |  |

- team_ids:

  The team_id(s) for which to return information.

- league_ids:

  The league_id(s) for which to return information.

- sport_ids:

  The sport_id(s) for which to return information.

- game_type:

  The game_type for which to return information.

- stat_group:

  Stat group for which to return information. Valid values include:

  |            |
  |------------|
  | stat_group |
  | hitting    |
  | pitching   |
  | fielding   |
  | catching   |
  | running    |
  | game       |
  | team       |
  | streak     |
  |            |

- limit:

  Number of records as the limit of the return.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| total_splits | integer | Total number of split records matching the query. |
| season | integer | Season year for the split. |
| date | character | Date of the game (YYYY-MM-DD). |
| is_home | logical | Whether the subject team was the home team. |
| rank | integer | Rank of the split within the high/low leaderboard. |
| game_innings | integer | Number of innings played in the game. |
| stat_at_bats | integer | Value of the sorted statistic (here at bats) for the split. |
| team_id | integer | MLB team id for the subject team. |
| team_name | character | Subject team name. |
| team_link | character | API relative link to the subject team. |
| opponent_id | integer | MLB team id for the opponent. |
| opponent_name | character | Opponent team name. |
| opponent_link | character | API relative link to the opponent team. |
| game_pk | integer | MLB game primary key. |
| game_link | character | API relative link to the game live feed. |
| game_number | integer | Game number within a day (1 unless a doubleheader). |
| game_day_night | character | Day/night designation of the game ('day' or 'night'). |
| game_content_link | character | API relative link to the game content endpoint. |
| home_team_id | integer | Home team id (populated for game-level org types). |
| home_team_name | character | Home team name (populated for game-level org types). |
| home_team_link | character | API relative link to the home team. |
| away_team_id | integer | Away team id (populated for game-level org types). |
| away_team_name | character | Away team name (populated for game-level org types). |
| away_team_link | character | API relative link to the away team. |
| combined_stats | logical | Whether the stat combines multiple split sources. |
| group_display_name | character | Stat group display name (e.g. 'hitting'). |
| game_type_id | character | Single-letter game type code (e.g. 'R'). |
| game_type_description | character | Game type description (e.g. 'Regular Season'). |
| sort_stat_name | character | Snake-case name of the sorted statistic (e.g. 'at_bats'). |
| sort_stat_lookup_param | character | API lookup parameter for the sorted statistic (e.g. 'atBats'). |
| sort_stat_is_counting | logical | Whether the sorted statistic is a counting stat. |
| sort_stat_label | character | Human-readable label of the sorted statistic (e.g. 'At bats'). |

## Examples

``` r
# \donttest{
  try(mlb_high_low_stats(org_type = 'Team', season = 2020, sort_stat = 'atBats'))
#> ── MLB High Low Stats data from MLB.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 14:09:35 UTC
#> # A tibble: 20 × 32
#>    total_splits season date      is_home  rank game_innings stat_at_bats
#>           <int>  <int> <chr>     <lgl>   <int>        <int>        <int>
#>  1           31   2020 2020-09-… FALSE       1            9           52
#>  2           31   2020 2020-09-… FALSE       2           12           49
#>  3           31   2020 2020-08-… FALSE       2            9           49
#>  4           31   2020 2020-08-… FALSE       2           11           49
#>  5           31   2020 2020-09-… FALSE       2            9           49
#>  6           30   2020 2020-08-… FALSE       1           11           96
#>  7           30   2020 2020-09-… FALSE       2           12           94
#>  8           30   2020 2020-08-… FALSE       3           11           91
#>  9           30   2020 2020-08-… FALSE       4           13           90
#> 10           30   2020 2020-08-… FALSE       4            9           90
#> 11           31   2020 2020-09-… TRUE        1            9           52
#> 12           31   2020 2020-08-… TRUE        2            9           49
#> 13           31   2020 2020-08-… TRUE        3           11           49
#> 14           31   2020 2020-09-… TRUE        4           12           49
#> 15           31   2020 2020-09-… TRUE        5            9           49
#> 16           30   2020 2020-08-… FALSE       1           11           96
#> 17           30   2020 2020-09-… FALSE       2           12           94
#> 18           30   2020 2020-08-… FALSE       3           11           91
#> 19           30   2020 2020-08-… FALSE       4           13           90
#> 20           30   2020 2020-08-… FALSE       5            9           90
#> # ℹ 25 more variables: team_id <int>, team_name <chr>, team_link <chr>,
#> #   opponent_id <int>, opponent_name <chr>, opponent_link <chr>,
#> #   game_pk <int>, game_link <chr>, game_number <int>,
#> #   game_day_night <chr>, game_content_link <chr>, home_team_id <int>,
#> #   home_team_name <chr>, home_team_link <chr>, away_team_id <int>,
#> #   away_team_name <chr>, away_team_link <chr>, combined_stats <lgl>,
#> #   group_display_name <chr>, game_type_id <chr>, …
# }
```

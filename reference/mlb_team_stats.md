# **MLB Team Individual Stats**

**MLB Team Individual Stats**

## Usage

``` r
mlb_team_stats(
  team_id = NULL,
  stat_type = NULL,
  game_type = NULL,
  stat_group = NULL,
  season = NULL,
  sport_ids = NULL
)
```

## Arguments

- team_id:

  Team ID to return information and ranking for a particular statistic
  for a particular team.

- stat_type:

  Stat type to return statistics for.

- game_type:

  Game type to return information for a particular statistic in a
  particular game type.

- stat_group:

  Stat group to return information and ranking for a particular
  statistic in a particular group.

- season:

  Year to return information and ranking for a particular statistic in a
  given year.

- sport_ids:

  The sport_id(s) to return information and ranking information for.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| season | character | Season year for the statistic. |
| games_played | integer | Games played. |
| ground_outs | integer | Ground outs. |
| air_outs | integer | Air outs (fly outs). |
| runs | integer | Runs scored. |
| doubles | integer | Doubles. |
| triples | integer | Triples. |
| home_runs | integer | Home runs. |
| strike_outs | integer | Strikeouts. |
| base_on_balls | integer | Walks (bases on balls). |
| intentional_walks | integer | Intentional walks. |
| hits | integer | Hits. |
| hit_by_pitch | integer | Times hit by pitch. |
| avg | character | Batting average. |
| at_bats | integer | At bats. |
| obp | character | On-base percentage. |
| slg | character | Slugging percentage. |
| ops | character | On-base plus slugging. |
| caught_stealing | integer | Times caught stealing. |
| stolen_bases | integer | Stolen bases. |
| stolen_base_percentage | character | Stolen base success percentage. |
| caught_stealing_percentage | character | Caught stealing percentage. |
| ground_into_double_play | integer | Grounded into double plays. |
| number_of_pitches | integer | Total pitches seen. |
| plate_appearances | integer | Plate appearances. |
| total_bases | integer | Total bases. |
| rbi | integer | Runs batted in. |
| left_on_base | integer | Runners left on base. |
| sac_bunts | integer | Sacrifice bunts. |
| sac_flies | integer | Sacrifice flies. |
| babip | character | Batting average on balls in play. |
| ground_outs_to_airouts | character | Ratio of ground outs to air outs. |
| catchers_interference | integer | Times reached on catcher's interference. |
| at_bats_per_home_run | character | At bats per home run. |
| team_id | integer | Team MLBAM ID. |
| team_name | character | Team name. |
| team_link | character | API link to the team. |
| type_display_name | character | Stat type display name. |
| group_display_name | character | Stat group display name. |

## Examples

``` r
# \donttest{
  try(mlb_team_stats(team_id = 137, stat_type = 'season', stat_group = 'hitting', season = 2021))
#> ── MLB Team Stats data from MLB.com ───────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:24:21 UTC
#> # A tibble: 1 × 39
#>   season games_played ground_outs air_outs  runs doubles triples
#>   <chr>         <int>       <int>    <int> <int>   <int>   <int>
#> 1 2021            162        1304     1403   804     271      25
#> # ℹ 32 more variables: home_runs <int>, strike_outs <int>,
#> #   base_on_balls <int>, intentional_walks <int>, hits <int>,
#> #   hit_by_pitch <int>, avg <chr>, at_bats <int>, obp <chr>, slg <chr>,
#> #   ops <chr>, caught_stealing <int>, stolen_bases <int>,
#> #   stolen_base_percentage <chr>, caught_stealing_percentage <chr>,
#> #   ground_into_double_play <int>, number_of_pitches <int>,
#> #   plate_appearances <int>, total_bases <int>, rbi <int>, …
# }
```

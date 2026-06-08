# **MLB Teams Stats**

**MLB Teams Stats**

## Usage

``` r
mlb_teams_stats(
  stat_type = NULL,
  game_type = NULL,
  stat_group = NULL,
  season = NULL,
  sport_ids = NULL,
  sort_stat = NULL,
  order = NULL
)
```

## Arguments

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

- sort_stat:

  Sort return based on stat.

- order:

  Order return based on either desc or asc.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| total_splits | integer | Total number of splits in the response. |
| season | character | Season year for the statistic. |
| rank | integer | Rank of the team for the sorted statistic. |
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
| splits_tied_with_offset | list | Teams tied at the offset boundary. |
| splits_tied_with_limit | list | Teams tied at the limit boundary. |
| type_display_name | character | Stat type display name. |
| group_display_name | character | Stat group display name. |

## Examples

``` r
# \donttest{
  try(mlb_teams_stats(stat_type = 'season', stat_group = 'hitting', season = 2021))
#> ── MLB Teams Stats data from MLB.com ──────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:20:41 UTC
#> # A tibble: 50 × 43
#>    total_splits season  rank games_played ground_outs air_outs  runs
#>           <int> <chr>  <int>        <int>       <int>    <int> <int>
#>  1          371 2021       1            2           8        6    10
#>  2          371 2021       2            2          14       19    18
#>  3          371 2021       3            2          14        8     9
#>  4          371 2021       4            1           4       17     7
#>  5          371 2021       5            2          18       15    19
#>  6          371 2021       6            1           9        7    13
#>  7          371 2021       7           63         546      495   500
#>  8          371 2021       8            1           8        4     9
#>  9          371 2021       9            3          24       26    23
#> 10          371 2021      10           66         593      594   438
#> # ℹ 40 more rows
#> # ℹ 36 more variables: doubles <int>, triples <int>, home_runs <int>,
#> #   strike_outs <int>, base_on_balls <int>, intentional_walks <int>,
#> #   hits <int>, hit_by_pitch <int>, avg <chr>, at_bats <int>,
#> #   obp <chr>, slg <chr>, ops <chr>, caught_stealing <int>,
#> #   stolen_bases <int>, stolen_base_percentage <chr>,
#> #   caught_stealing_percentage <chr>, ground_into_double_play <int>, …
# }
```

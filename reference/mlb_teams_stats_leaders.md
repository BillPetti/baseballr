# **MLB Teams Stats Leaders**

**MLB Teams Stats Leaders**

## Usage

``` r
mlb_teams_stats_leaders(
  leader_categories = NULL,
  leader_game_types = NULL,
  sit_codes = NULL,
  stat_group = NULL,
  season = NULL,
  league_id = NULL,
  sport_id = NULL,
  start_date = NULL,
  end_date = NULL,
  stat_type = NULL,
  limit = 1000
)
```

## Arguments

- leader_categories:

  League leader category to return information and ranking for a
  particular statistic.

- leader_game_types:

  Game type to return information and ranking for a particular statistic
  in a particular game type.

- sit_codes:

  Situation code to return information and ranking for a particular
  statistic in a particular game type.

- stat_group:

  Stat group to return information and ranking for a particular
  statistic in a particular group.

- season:

  Year to return information and ranking for a particular statistic in a
  given year.

- league_id:

  League ID to return statistics for a given league. Default to
  "Qualified" player pool.

- sport_id:

  The sport_id to return information and ranking information for.

- start_date:

  Start date to return information and ranking for a particular
  statistic for a particular date range. Format: MM/DD/YYYY *start_date
  must be coupled with end_date and byDateRange stat_type*

- end_date:

  End date to return information and ranking for a particular statistic
  for a particular date range. Format: MM/DD/YYYY *end_date must be
  coupled with start_date and byDateRange stat_type*

- stat_type:

  The stat_type to return information and ranking for a particular
  statistic for a particular stat type.

- limit:

  A limit to limit return to a particular number of records.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| leader_category | character | Team leader category (e.g., homeRuns). |
| rank | integer | Rank within the team leaderboard. |
| value | character | Statistic value for the team. |
| season | character | Season year. |
| team_id | integer | Team MLBAM ID. |
| team_name | character | Team name. |
| team_link | character | API link to the team. |
| stat_group | character | Stat group (e.g., hitting). |
| total_splits | integer | Total number of splits in the leaderboard. |
| game_type_id | character | Game type code (e.g., R for regular season). |
| game_type_description | character | Game type description. |

## Examples

``` r
# \donttest{
 try(mlb_teams_stats_leaders(leader_categories='homeRuns',sport_id=1, season = 2021))
#> ── MLB Teams Stats Leaders data from MLB.com ──────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 01:57:35 UTC
#> # A tibble: 90 × 11
#>    leader_category  rank value season team_id team_name        team_link
#>    <chr>           <int> <chr> <chr>    <int> <chr>            <chr>    
#>  1 homeRuns            1 262   2021       141 Toronto Blue Ja… /api/v1/…
#>  2 homeRuns            2 241   2021       137 San Francisco G… /api/v1/…
#>  3 homeRuns            3 239   2021       144 Atlanta Braves   /api/v1/…
#>  4 homeRuns            4 237   2021       119 Los Angeles Dod… /api/v1/…
#>  5 homeRuns            5 228   2021       142 Minnesota Twins  /api/v1/…
#>  6 homeRuns            6 222   2021       113 Cincinnati Reds  /api/v1/…
#>  7 homeRuns            6 222   2021       147 New York Yankees /api/v1/…
#>  8 homeRuns            6 222   2021       139 Tampa Bay Rays   /api/v1/…
#>  9 homeRuns            9 221   2021       117 Houston Astros   /api/v1/…
#> 10 homeRuns           10 219   2021       111 Boston Red Sox   /api/v1/…
#> # ℹ 80 more rows
#> # ℹ 4 more variables: stat_group <chr>, total_splits <int>,
#> #   game_type_id <chr>, game_type_description <chr>
# }
```

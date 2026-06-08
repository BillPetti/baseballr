# **MLB Stat High/Low Types**

**MLB Stat High/Low Types**

## Usage

``` r
mlb_high_low_types()
```

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| stat_name | character | Snake-case name of the statistic (e.g. 'at_bats'). |
| stat_lookup_param | character | API lookup parameter for the statistic (e.g. 'atBats'). |
| is_counting | logical | Whether the statistic is a counting stat. |
| stat_label | character | Human-readable label of the statistic (e.g. 'At bats'). |
| stat_groups | list | List-column of stat group display names the stat belongs to. |
| org_types | list | List-column of organization types the stat applies to (e.g. PLAYER). |
| high_low_types | list | List-column of high/low aggregation types (e.g. PLAYER, TEAM, GAME). |

## Examples

``` r
# \donttest{
  try(mlb_high_low_types())
#> ── MLB High Low Types data from MLB.com ───────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:20:16 UTC
#> # A tibble: 43 × 7
#>    stat_name        stat_lookup_param is_counting stat_label stat_groups
#>    <chr>            <chr>             <lgl>       <chr>      <list<chr>>
#>  1 at_bats          atBats            TRUE        At bats            [2]
#>  2 total_plate_app… plateAppearances  TRUE        Total pla…         [1]
#>  3 runs             runs              TRUE        Runs               [1]
#>  4 runs_batted_in   rbi               TRUE        Runs batt…         [1]
#>  5 home_team_runs   runs              TRUE        Home team…         [1]
#>  6 away_team_runs   runs              TRUE        Away team…         [1]
#>  7 hits             hits              TRUE        Hits               [1]
#>  8 hits_risp        hitsRisp          TRUE        Hits risp          [1]
#>  9 home_team_hits   hits              TRUE        Home team…         [1]
#> 10 away_team_hits   hits              TRUE        Away team…         [1]
#> # ℹ 33 more rows
#> # ℹ 2 more variables: org_types <list>, high_low_types <list>
# }
```

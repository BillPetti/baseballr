# **MLB Award Recipients**

**MLB Award Recipients**

## Usage

``` r
mlb_awards_recipient(
  award_id = NULL,
  sport_id = NULL,
  league_id = NULL,
  season = NULL
)
```

## Arguments

- award_id:

  award_id to return a directory of players for a given award.

- sport_id:

  sport_id to return a directory of players for a given aware in a
  specific sport.

- league_id:

  league_id(s) to return a directory of players for a given award in a
  specific league. Format '103,104'

- season:

  Year(s) to return a directory of players for a given award in a given
  season.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| award_id | character | Award identifier code. |
| award_name | character | Award name. |
| date | character | Date the award was given (YYYY-MM-DD). |
| season | character | Season the award was given (YYYY). |
| votes | integer | Number of votes received. |
| notes | character | Additional notes about the recipient. |
| player_id | integer | MLB player ID of the recipient. |
| player_link | character | MLB Stats API relative player link. |
| player_name_first_last | character | Recipient name in first-last order. |
| player_primary_position_code | character | Recipient primary fielding position code. |
| player_primary_position_name | character | Recipient primary fielding position name. |
| player_primary_position_type | character | Recipient primary position type. |
| player_primary_position_abbreviation | character | Recipient primary position abbreviation. |
| team_id | integer | MLB team ID of the recipient. |
| team_link | character | MLB Stats API relative team link. |

## Examples

``` r
# \donttest{
  try(mlb_awards_recipient(award_id = 'MLBHOF', season = 2020))
#> ── MLB Awards Recipient data from MLB.com ─────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:56:46 UTC
#> # A tibble: 4 × 15
#>   award_id award_name   date    season votes notes player_id player_link
#>   <chr>    <chr>        <chr>   <chr>  <int> <chr>     <int> <chr>      
#> 1 MLBHOF   Hall Of Fame 2020-0… 2020      12 Mode…    692968 /api/v1/pe…
#> 2 MLBHOF   Hall Of Fame 2020-0… 2020      13 Mode…    122247 /api/v1/pe…
#> 3 MLBHOF   Hall Of Fame 2020-0… 2020     396 NA       116539 /api/v1/pe…
#> 4 MLBHOF   Hall Of Fame 2020-0… 2020     304 NA       123833 /api/v1/pe…
#> # ℹ 7 more variables: player_name_first_last <chr>,
#> #   player_primary_position_code <chr>,
#> #   player_primary_position_name <chr>,
#> #   player_primary_position_type <chr>,
#> #   player_primary_position_abbreviation <chr>, team_id <int>,
#> #   team_link <chr>
# }
```

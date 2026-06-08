# **MLB Leagues**

**MLB Leagues**

## Usage

``` r
mlb_league(seasons = NULL, sport_id = NULL, league_id = NULL)
```

## Arguments

- seasons:

  Year(s) to return to return league information for.

- sport_id:

  The sport_id to return league information for.

- league_id:

  The league_id(s) to return league information for.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| league_id | integer | MLB league id (e.g. 103 for AL, 104 for NL). |
| league_name | character | League name (e.g. 'American League'). |
| league_link | character | API relative link to the league. |
| league_abbreviation | character | League abbreviation (e.g. 'AL'). |
| league_name_short | character | Short league name (e.g. 'American'). |
| league_season_state | character | Season state (e.g. 'offseason', 'regular'). |
| league_has_wild_card | logical | Whether the league uses a wild card. |
| league_has_split_season | logical | Whether the league has a split season. |
| league_num_games | integer | Scheduled number of regular season games. |
| league_has_playoff_points | logical | Whether the league awards playoff points. |
| league_num_teams | integer | Number of teams in the league. |
| league_num_wildcard_teams | integer | Number of wild card teams. |
| league_season | character | Season year for the league record. |
| league_org_code | character | Organization code (e.g. 'AL'). |
| league_conferences_in_use | logical | Whether conferences are used. |
| league_divisions_in_use | logical | Whether divisions are used. |
| league_sort_order | integer | Display sort order for the league. |
| league_active | logical | Whether the league is active. |
| season_date_info_season_id | character | Season identifier for the date info block. |
| season_date_info_pre_season_start_date | character | Preseason start date (YYYY-MM-DD). |
| season_date_info_pre_season_end_date | character | Preseason end date (YYYY-MM-DD). |
| season_date_info_season_start_date | character | Season start date (YYYY-MM-DD). |
| season_date_info_spring_start_date | character | Spring training start date (YYYY-MM-DD). |
| season_date_info_spring_end_date | character | Spring training end date (YYYY-MM-DD). |
| season_date_info_regular_season_start_date | character | Regular season start date (YYYY-MM-DD). |
| season_date_info_last_date1st_half | character | Last date of the first half (YYYY-MM-DD). |
| season_date_info_all_star_date | character | All-Star Game date (YYYY-MM-DD). |
| season_date_info_first_date2nd_half | character | First date of the second half (YYYY-MM-DD). |
| season_date_info_regular_season_end_date | character | Regular season end date (YYYY-MM-DD). |
| season_date_info_post_season_start_date | character | Postseason start date (YYYY-MM-DD). |
| season_date_info_post_season_end_date | character | Postseason end date (YYYY-MM-DD). |
| season_date_info_season_end_date | character | Season end date (YYYY-MM-DD). |
| season_date_info_offseason_start_date | character | Offseason start date (YYYY-MM-DD). |
| season_date_info_off_season_end_date | character | Offseason end date (YYYY-MM-DD). |
| season_date_info_season_level_gameday_type | character | Season-level Gameday data type code. |
| season_date_info_game_level_gameday_type | character | Game-level Gameday data type code. |
| season_date_info_qualifier_plate_appearances | numeric | Plate appearances per game needed to qualify. |
| season_date_info_qualifier_outs_pitched | integer | Outs pitched per game needed to qualify. |
| sport_id | integer | Sport id associated with the league (1 for MLB). |
| sport_link | character | API relative link to the sport. |

## Examples

``` r
# \donttest{
  try(mlb_league(seasons = 2021, sport_id = 1))
#> ── MLB League data from MLB.com ───────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-08 03:45:57 UTC
#> # A tibble: 4 × 40
#>   league_id league_name       league_link        league_abbreviation
#>       <int> <chr>             <chr>              <chr>              
#> 1       103 American League   /api/v1/league/103 AL                 
#> 2       104 National League   /api/v1/league/104 NL                 
#> 3       114 Cactus League     /api/v1/league/114 CL                 
#> 4       115 Grapefruit League /api/v1/league/115 GL                 
#> # ℹ 36 more variables: league_name_short <chr>,
#> #   league_season_state <chr>, league_has_wild_card <lgl>,
#> #   league_has_split_season <lgl>, league_num_games <int>,
#> #   league_has_playoff_points <lgl>, league_num_teams <int>,
#> #   league_num_wildcard_teams <int>, league_season <chr>,
#> #   league_org_code <chr>, league_conferences_in_use <lgl>,
#> #   league_divisions_in_use <lgl>, league_sort_order <int>, …
# }
```

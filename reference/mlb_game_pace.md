# **Retrieve game pace metrics for major and minor league**

**Retrieve game pace metrics for major and minor league**

## Usage

``` r
mlb_game_pace(
  season,
  league_ids = NULL,
  sport_ids = NULL,
  team_ids = NULL,
  game_type = NULL,
  venue_ids = NULL,
  org_type = NULL,
  start_date = NULL,
  end_date = NULL
)
```

## Arguments

- season:

  Year for which to return information (*Required*).

- league_ids:

  The league_id(s) for which to return information.

- sport_ids:

  The sport_id(s) for which to return information.

- team_ids:

  The team_id(s) for which to return information.

- game_type:

  The game_type for which to return information.

- venue_ids:

  Venue directorial information based venue_id.

- org_type:

  pace of game metrics based on team ('T'), league ('L') or sport('S')

- start_date:

  Date of first game for which you want data. Format must be in
  MM/DD/YYYY format.

- end_date:

  Date of last game for which you want data. Format must be in
  MM/DD/YYYY format.

## Value

Returns a tibble with the following columns

|  |  |  |
|----|----|----|
| col_name | types | description |
| hits_per9inn | numeric | Hits per 9 innings. |
| runs_per9inn | numeric | Runs per 9 innings. |
| pitches_per9inn | numeric | Pitches per 9 innings. |
| plate_appearances_per9inn | numeric | Plate appearances per 9 innings. |
| hits_per_game | numeric | Hits per game. |
| runs_per_game | numeric | Runs per game. |
| innings_played_per_game | numeric | Innings played per game. |
| pitches_per_game | numeric | Pitches per game. |
| pitchers_per_game | numeric | Pitchers used per game. |
| plate_appearances_per_game | numeric | Plate appearances per game. |
| total_game_time | character | Total game time (HHH:MM:SS). |
| total_innings_played | numeric | Total innings played. |
| total_hits | integer | Total hits. |
| total_runs | integer | Total runs. |
| total_plate_appearances | integer | Total plate appearances. |
| total_pitchers | integer | Total pitchers used. |
| total_pitches | integer | Total pitches thrown. |
| total_games | integer | Total games. |
| total7inn_games | integer | Total 7-inning games. |
| total9inn_games | integer | Total 9-inning games. |
| total_extra_inn_games | integer | Total extra-inning games. |
| time_per_game | character | Average time per game (HH:MM:SS). |
| time_per_pitch | character | Average time per pitch (HH:MM:SS). |
| time_per_hit | character | Average time per hit (HH:MM:SS). |
| time_per_run | character | Average time per run (HH:MM:SS). |
| time_per_plate_appearance | character | Average time per plate appearance (HH:MM:SS). |
| time_per9inn | character | Average time per 9 innings (HH:MM:SS). |
| time_per77plate_appearances | character | Average time per 77 plate appearances. |
| total_extra_inn_time | character | Total extra-inning time (HHH:MM:SS). |
| time_per7inn_game | character | Average time per 7-inning game (HH:MM:SS). |
| time_per7inn_game_without_extra_inn | character | Average time per 7-inning game excl. extras. |
| total7inn_games_scheduled | integer | Total 7-inning games scheduled. |
| total7inn_games_without_extra_inn | integer | Total 7-inning games without extra innings. |
| total9inn_games_without_extra_inn | integer | Total 9-inning games without extra innings. |
| total9inn_games_scheduled | integer | Total 9-inning games scheduled. |
| hits_per_run | numeric | Hits per run. |
| pitches_per_pitcher | numeric | Pitches per pitcher. |
| season | character | Season (YYYY). |
| total9inn_games_completed_early | integer | 9-inning games completed early. |
| total7inn_games_completed_early | integer | 7-inning games completed early. |
| sport_id | integer | MLB sport ID. |
| sport_code | character | Sport code (e.g. mlb, aaa). |
| sport_link | character | MLB Stats API relative sport link. |
| pr_portal_calculated_fields_total7inn_games | integer | Portal-calculated total 7-inning games. |
| pr_portal_calculated_fields_total9inn_games | integer | Portal-calculated total 9-inning games. |
| pr_portal_calculated_fields_total_extra_inn_games | integer | Portal-calculated total extra-inning games. |
| pr_portal_calculated_fields_time_per7inn_game | character | Portal-calculated time per 7-inning game. |
| pr_portal_calculated_fields_time_per9inn_game | character | Portal-calculated time per 9-inning game. |
| pr_portal_calculated_fields_time_per_extra_inn_game | character | Portal-calculated time per extra-inning game. |

## Examples

``` r
# \donttest{
  try(mlb_game_pace(season = 2021, start_date = "09/14/2021", end_date = "09/16/2021"))
#> ── MLB Game Pace data from MLB.com ────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:57:00 UTC
#> # A tibble: 7 × 49
#>   hits_per9inn runs_per9inn pitches_per9inn plate_appearances_per9inn
#>          <dbl>        <dbl>           <dbl>                     <dbl>
#> 1         17.8        10.2             304.                      77.4
#> 2         18.3        10.8             308.                      78.7
#> 3         16.4         9.71            300.                      76.9
#> 4         15.9         9.25            302.                      77.1
#> 5         16.8        10.9             312.                      79.7
#> 6         17.3        12.7             166.                      81.8
#> 7         18.5        12.1             309.                      81.4
#> # ℹ 45 more variables: hits_per_game <dbl>, runs_per_game <dbl>,
#> #   innings_played_per_game <dbl>, pitches_per_game <dbl>,
#> #   pitchers_per_game <dbl>, plate_appearances_per_game <dbl>,
#> #   total_game_time <chr>, total_innings_played <dbl>,
#> #   total_hits <int>, total_runs <int>, total_plate_appearances <int>,
#> #   total_pitchers <int>, total_pitches <int>, total_games <int>,
#> #   total7inn_games <int>, total9inn_games <int>, …
# }
```

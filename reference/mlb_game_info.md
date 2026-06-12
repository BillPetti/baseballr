# **Retrieve additional game information for major and minor league games**

**Retrieve additional game information for major and minor league
games**

## Usage

``` r
mlb_game_info(game_pk)
```

## Arguments

- game_pk:

  The unique game_pk identifier for the game

## Value

Returns a tibble that includes supplemental information, such as
weather, official scorer, attendance, etc., for the game_pk provided

|                 |           |                                    |
|-----------------|-----------|------------------------------------|
| col_name        | types     | description                        |
| game_date       | character | Game date (YYYY-MM-DD).            |
| game_pk         | numeric   | Unique game identifier.            |
| venue_name      | character | Stadium name.                      |
| venue_id        | integer   | Venue ID.                          |
| temperature     | character | Game-time temperature (degrees F). |
| other_weather   | character | Weather condition description.     |
| wind            | character | Wind speed and direction.          |
| attendance      | character | Reported game attendance.          |
| start_time      | character | First-pitch local start time.      |
| elapsed_time    | character | Total elapsed game time (H:MM).    |
| game_id         | character | Human-readable game ID slug.       |
| game_type       | character | Game type code (R, P, etc.).       |
| home_sport_code | character | Home sport code (always 'mlb').    |
| official_scorer | character | Official scorer name.              |
| date            | character | Long-form game date label.         |
| status_ind      | character | Game status code.                  |
| home_league_id  | integer   | Home team league ID.               |
| gameday_sw      | character | Gameday data type code.            |

## Examples

``` r
# \donttest{
  try(mlb_game_info(game_pk = 566001))
#> ── MLB Game Info data from MLB.com ────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:23:48 UTC
#> # A tibble: 1 × 18
#>   game_date  game_pk venue_name venue_id temperature other_weather wind 
#>   <chr>        <dbl> <chr>         <int> <chr>       <chr>         <chr>
#> 1 2019-04-29  566001 Citi Field     3289 51          Cloudy        10 m…
#> # ℹ 11 more variables: attendance <chr>, start_time <chr>,
#> #   elapsed_time <chr>, game_id <chr>, game_type <chr>,
#> #   home_sport_code <chr>, official_scorer <chr>, date <chr>,
#> #   status_ind <chr>, home_league_id <int>, gameday_sw <chr>
# }
```

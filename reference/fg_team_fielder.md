# **Scrape Team Fielder Leaderboards from FanGraphs**

**Scrape Team Fielder Leaderboards from FanGraphs**

## Usage

``` r
fg_team_fielder(
  age = "",
  pos = "all",
  stats = "fld",
  lg = "all",
  qual = "0",
  startseason = "2023",
  endseason = "2023",
  startdate = "",
  enddate = "",
  month = "0",
  hand = "",
  team = "0,ts",
  pageitems = "1000",
  pagenum = "1",
  ind = "0",
  rost = "0",
  players = "",
  type = "1",
  postseason = "",
  sortdir = "default",
  sortstat = "Defense"
)
```

## Arguments

- age:

  (integer) Age of players

- pos:

  (character) Position of players, defaults to "all". To exclude
  pitchers, use "np".

- stats:

  (character) Statistic to return. Defaults to "bat".

- lg:

  (character) League to return. Defaults to "all". Options are "al",
  "nl", or "all".

- qual:

  (character) Whether you want only batters/pitchers that qualified in a
  given season, or the minimum number of plate appearances for
  inclusion. If you only want qualified hitters, use qual. If a minimum
  number of plate appearaces/innings pitched, use the number desired.
  Defaults to "y".

- startseason:

  (character) Season for which you want to scrape the data.

- endseason:

  (character) Last season for which you want data.

- startdate:

  (character) Start date for which you want data.

- enddate:

  (character) End date for which you want data.

- month:

  (character) Month for which you want data.

- hand:

  (character) Handedness of batter. Options are "L", "R", or "B". Empty
  string returns all.

- team:

  (character) Teams for which you want data, comma separated.

- pageitems:

  (character) Number of items per page.

- pagenum:

  (character) Page number.

- ind:

  (character) Whether or not to break the seasons out individual, or
  roll them up together. 1 = split seasons, 0 = aggregate seasons.

- rost:

  (character) Whether or not to include players on the roster. 1 =
  include, 0 = exclude.

- players:

  (character) Whether or not to include players on the roster. 1 =
  include only active roster players, 0 = exclude.

- type:

  (character) Defaults to 8, which is the standard leaderboard. The
  values for the leaderboards appear to go to from type = 0 to 48+,
  which correspond to links on the leaderboard page.

- postseason:

  (logical) Whether or not to include postseason data. TRUE = include
  postseason, FALSE = exclude postseason.

- sortdir:

  (character) Sort direction. Options are "asc" or "desc" or "default".

- sortstat:

  (character) Sort by stat. Default is "Defense".

## Value

A data frame of fielder data.

|               |           |                                                   |
|---------------|-----------|---------------------------------------------------|
| col_name      | types     | description                                       |
| Season        | integer   | Season (YYYY).                                    |
| team_name     | character | Team name.                                        |
| SeasonMin     | integer   | First season in the queried span.                 |
| SeasonMax     | integer   | Last season in the queried span.                  |
| Pos           | character | Primary position.                                 |
| Position      | character | Position played.                                  |
| G             | integer   | Games played.                                     |
| GS            | integer   | Games started.                                    |
| Inn           | integer   | Innings played in the field.                      |
| PO            | integer   | Putouts.                                          |
| A             | integer   | Assists.                                          |
| E             | integer   | Errors.                                           |
| FE            | integer   | Fielding errors.                                  |
| TE            | integer   | Throwing errors.                                  |
| DP            | integer   | Double plays.                                     |
| DPS           | integer   | Double plays started.                             |
| DPT           | integer   | Double plays turned.                              |
| DPF           | integer   | Double plays finished.                            |
| Scp           | integer   | Scoops (first-base picks).                        |
| SB            | integer   | Stolen bases.                                     |
| CS            | integer   | Caught stealing.                                  |
| PB            | integer   | Passed balls.                                     |
| WP            | integer   | Wild pitches.                                     |
| FP            | numeric   | Fielding percentage.                              |
| rSB           | integer   | Stolen-base runs (catcher arm).                   |
| rGDP          | integer   | Double-play runs.                                 |
| rARM          | integer   | Outfield-arm runs.                                |
| rGFP          | integer   | Good-fielding-play runs.                          |
| rPM           | integer   | Plus/minus range runs.                            |
| rSZ           | numeric   | Strike-zone (framing) runs.                       |
| rTS           | integer   | Team-stolen-base runs.                            |
| rCERA         | integer   | Catcher-ERA runs.                                 |
| DRS           | integer   | Defensive Runs Saved.                             |
| BIZ           | integer   | Balls hit in defensive zone.                      |
| Plays         | integer   | Plays made in zone.                               |
| RZR           | numeric   | Revised Zone Rating.                              |
| OOZ           | integer   | Plays made out of zone.                           |
| ARM           | numeric   | Outfield-arm runs (UZR component).                |
| DPR           | numeric   | Double-play runs (UZR component).                 |
| RngR          | numeric   | Range runs (UZR component).                       |
| ErrR          | numeric   | Error runs (UZR component).                       |
| UZR           | numeric   | Ultimate Zone Rating.                             |
| UZR_150       | numeric   | Ultimate Zone Rating per 150 defensive games.     |
| Defense       | numeric   | Total defensive value (runs above average).       |
| CStrikes      | numeric   | Catcher framing called strikes above average.     |
| CFraming      | numeric   | Catcher framing runs.                             |
| OAA           | integer   | Outs Above Average (Statcast).                    |
| rFRP          | integer   | Range component of Fielding Run Prevention.       |
| aFRP          | integer   | Arm component of Fielding Run Prevention.         |
| dFRP          | integer   | Double-play component of Fielding Run Prevention. |
| bFRP          | integer   | Bunt component of Fielding Run Prevention.        |
| tFRP          | integer   | Throwing component of Fielding Run Prevention.    |
| fFRP          | integer   | Framing component of Fielding Run Prevention.     |
| FRP           | integer   | Total Fielding Run Prevention.                    |
| Q             | numeric   | Quality of contact / quality score.               |
| TInn          | numeric   | Total innings played in the field.                |
| positionDB    | character | Position code from the database.                  |
| teamid        | integer   | FanGraphs team ID.                                |
| team_name_abb | character | Team name abbreviation.                           |
| playerTeamId  | integer   | FanGraphs player-team ID.                         |

## Examples

``` r
# \donttest{
  try(fg_team_fielder(startseason = 2023, endseason = 2023, qual = 150))
#> ── MLB Team Fielding data from FanGraphs.com ──────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:41:51 UTC
#> # A tibble: 30 × 60
#>    Season team_name SeasonMin SeasonMax Pos   Position     G    GS   Inn
#>     <int> <chr>         <int>     <int> <chr> <chr>    <int> <int> <int>
#>  1   2023 Brewers        2023      2023 ---   ---       2225  1458 12987
#>  2   2023 Rangers        2023      2023 ---   ---       2121  1458 12927
#>  3   2023 Pirates        2023      2023 ---   ---       2298  1458 12870
#>  4   2023 Royals         2023      2023 ---   ---       2210  1458 12681
#>  5   2023 Giants         2023      2023 ---   ---       2319  1458 12912
#>  6   2023 Blue Jays      2023      2023 ---   ---       2231  1458 13065
#>  7   2023 Mariners       2023      2023 ---   ---       2166  1458 13044
#>  8   2023 Diamondb…      2023      2023 ---   ---       2235  1458 12918
#>  9   2023 Yankees        2023      2023 ---   ---       2160  1458 12957
#> 10   2023 Padres         2023      2023 ---   ---       2128  1458 12969
#> # ℹ 20 more rows
#> # ℹ 51 more variables: PO <int>, A <int>, E <int>, FE <int>, TE <int>,
#> #   DP <int>, DPS <int>, DPT <int>, DPF <int>, Scp <int>, SB <int>,
#> #   CS <int>, PB <int>, WP <int>, FP <dbl>, rSB <int>, rGDP <int>,
#> #   rARM <int>, rGFP <int>, rPM <int>, rSZ <dbl>, rTS <int>,
#> #   rCERA <int>, DRS <int>, BIZ <int>, Plays <int>, RZR <dbl>,
#> #   OOZ <int>, ARM <dbl>, DPR <dbl>, RngR <dbl>, ErrR <dbl>, …
# }
```

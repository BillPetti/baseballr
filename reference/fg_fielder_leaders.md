# **Scrape Fielder Leaderboards from FanGraphs**

This function allows you to scrape all leaderboard statistics (basic and
advanced) from FanGraphs.com.

## Usage

``` r
fg_fielder_leaders(
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
  team = "0",
  pageitems = "10000",
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

|  |  |  |
|----|----|----|
| col_name | types | description |
| Season | integer | Season (YYYY). |
| team_name | character | Team name. |
| xMLBAMID | integer | MLBAM player ID. |
| PlayerNameRoute | character | Player name URL slug used by FanGraphs. |
| PlayerName | character | Player name. |
| playerid | integer | FanGraphs player ID. |
| SeasonMin | integer | First season in the queried span. |
| SeasonMax | integer | Last season in the queried span. |
| Pos | character | Primary position. |
| Position | character | Position played. |
| G | integer | Games played. |
| GS | integer | Games started. |
| Inn | numeric | Innings played in the field. |
| PO | integer | Putouts. |
| A | integer | Assists. |
| E | integer | Errors. |
| FE | integer | Fielding errors. |
| TE | integer | Throwing errors. |
| DP | integer | Double plays. |
| DPS | integer | Double plays started. |
| DPT | integer | Double plays turned. |
| DPF | integer | Double plays finished. |
| SB | integer | Stolen bases. |
| CS | integer | Caught stealing. |
| PB | integer | Passed balls. |
| WP | integer | Wild pitches. |
| FP | numeric | Fielding percentage. |
| rSB | integer | Stolen-base runs (catcher arm). |
| rGFP | integer | Good-fielding-play runs. |
| rSZ | numeric | Strike-zone (framing) runs. |
| rCERA | integer | Catcher-ERA runs. |
| DRS | integer | Defensive Runs Saved. |
| Defense | numeric | Total defensive value (runs above average). |
| CStrikes | numeric | Catcher framing called strikes above average. |
| CFraming | numeric | Catcher framing runs. |
| dFRP | integer | Double-play component of Fielding Run Prevention. |
| bFRP | integer | Bunt component of Fielding Run Prevention. |
| tFRP | integer | Throwing component of Fielding Run Prevention. |
| fFRP | integer | Framing component of Fielding Run Prevention. |
| FRP | integer | Total Fielding Run Prevention. |
| Q | numeric | Quality of contact / quality score. |
| TInn | numeric | Total innings played in the field. |
| positionDB | character | Position code from the database. |
| teamid | integer | FanGraphs team ID. |
| team_name_abb | character | Team name abbreviation. |
| playerTeamId | integer | FanGraphs player-team ID. |
| rGDP | integer | Double-play runs. |
| rPM | integer | Plus/minus range runs. |
| BIZ | integer | Balls hit in defensive zone. |
| Plays | integer | Plays made in zone. |
| RZR | numeric | Revised Zone Rating. |
| OOZ | integer | Plays made out of zone. |
| DPR | numeric | Double-play runs (UZR component). |
| RngR | numeric | Range runs (UZR component). |
| ErrR | numeric | Error runs (UZR component). |
| UZR | numeric | Ultimate Zone Rating. |
| UZR_150 | numeric | Ultimate Zone Rating per 150 defensive games. |
| OAA | integer | Outs Above Average (Statcast). |
| rFRP | integer | Range component of Fielding Run Prevention. |
| aFRP | integer | Arm component of Fielding Run Prevention. |
| rARM | integer | Outfield-arm runs. |
| ARM | numeric | Outfield-arm runs (UZR component). |
| Scp | integer | Scoops (first-base picks). |

## Examples

``` r
# \donttest{
  try(fg_fielder_leaders(startseason = 2023, endseason = 2023))
#> ── MLB Player Fielding Leaders data from FanGraphs.com ─────────────────
#> ℹ Data updated: 2026-06-12 11:56:07 UTC
#> # A tibble: 2,293 × 63
#>    Season team_name xMLBAMID PlayerNameRoute   PlayerName       playerid
#>     <int> <chr>        <int> <chr>             <chr>               <int>
#>  1   2023 SFG         672275 Patrick Bailey    Patrick Bailey      27478
#>  2   2023 - - -       595978 Austin Hedges     Austin Hedges       12976
#>  3   2023 MIL         661388 William Contreras William Contrer…    20503
#>  4   2023 NYM         682626 Francisco Alvarez Francisco Alvar…    26121
#>  5   2023 CHC         621020 Dansby Swanson    Dansby Swanson      18314
#>  6   2023 TEX         641680 Jonah Heim        Jonah Heim          16930
#>  7   2023 ATL         669221 Sean Murphy       Sean Murphy         19352
#>  8   2023 TOR         672386 Alejandro Kirk    Alejandro Kirk      22581
#>  9   2023 SEA         663728 Cal Raleigh       Cal Raleigh         21534
#> 10   2023 COL         678662 Ezequiel Tovar    Ezequiel Tovar      24064
#> # ℹ 2,283 more rows
#> # ℹ 57 more variables: SeasonMin <int>, SeasonMax <int>, Pos <chr>,
#> #   Position <chr>, G <int>, GS <int>, Inn <dbl>, PO <int>, A <int>,
#> #   E <int>, FE <int>, TE <int>, DP <int>, DPS <int>, DPT <int>,
#> #   DPF <int>, SB <int>, CS <int>, PB <int>, WP <int>, FP <dbl>,
#> #   rSB <int>, rGFP <int>, rSZ <dbl>, rCERA <int>, DRS <int>,
#> #   Defense <dbl>, CStrikes <dbl>, CFraming <dbl>, dFRP <int>, …
# }
```

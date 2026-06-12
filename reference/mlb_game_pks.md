# **Get MLB Game Info by Date and Level**

Find game_pk values for professional baseball games (major and minor
leagues) via the MLB api <https://www.mlb.com/>

## Usage

``` r
mlb_game_pks(date, level_ids = c(1))
```

## Arguments

- date:

  The date for which you want to find game_pk values for MLB games

- level_ids:

  A numeric vector with ids for each level where game_pks are desired.
  See below for a reference of level ids.

## Value

Returns a tibble that includes game_pk values and additional information
for games scheduled or played with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_pk | integer | Unique game identifier. |
| gameGuid | character | Globally unique game identifier. |
| link | character | MLB Stats API relative game link. |
| gameType | character | Game type code (R, P, etc.). |
| season | character | Season (YYYY). |
| gameDate | character | Game date-time (ISO 8601, UTC). |
| officialDate | character | Official game date (YYYY-MM-DD). |
| isTie | logical | Whether the game ended in a tie. |
| gameNumber | integer | Game number within a doubleheader. |
| publicFacing | logical | Whether the game is public-facing. |
| doubleHeader | character | Doubleheader indicator (N/Y/S). |
| gamedayType | character | Gameday data type code. |
| tiebreaker | character | Tiebreaker indicator. |
| calendarEventID | character | Calendar event identifier. |
| seasonDisplay | character | Display season (YYYY). |
| dayNight | character | Day/night designation. |
| scheduledInnings | integer | Number of scheduled innings. |
| reverseHomeAwayStatus | logical | Whether home/away designation is reversed. |
| inningBreakLength | integer | Length of the inning break (seconds). |
| gamesInSeries | integer | Total games in the series. |
| seriesGameNumber | integer | Game number within the series. |
| seriesDescription | character | Series description. |
| recordSource | character | Source of the record data. |
| ifNecessary | character | Whether the game is played only if necessary. |
| ifNecessaryDescription | character | If-necessary description. |
| status.abstractGameState | character | Abstract game state (e.g. Final). |
| status.codedGameState | character | Coded game state. |
| status.detailedState | character | Detailed game state. |
| status.statusCode | character | Game status code. |
| status.startTimeTBD | logical | Whether the start time is TBD. |
| status.abstractGameCode | character | Abstract game code. |
| teams.away.score | integer | Away team score. |
| teams.away.isWinner | logical | Whether the away team won. |
| teams.away.splitSquad | logical | Whether the away team is a split squad. |
| teams.away.seriesNumber | integer | Away team series number. |
| teams.away.team.id | integer | Away team MLB ID. |
| teams.away.team.name | character | Away team name. |
| teams.away.team.link | character | MLB Stats API relative away team link. |
| teams.away.leagueRecord.wins | integer | Away team league-record wins. |
| teams.away.leagueRecord.losses | integer | Away team league-record losses. |
| teams.away.leagueRecord.ties | integer | Away team league-record ties. |
| teams.away.leagueRecord.pct | character | Away team winning percentage. |
| teams.home.score | integer | Home team score. |
| teams.home.isWinner | logical | Whether the home team won. |
| teams.home.splitSquad | logical | Whether the home team is a split squad. |
| teams.home.seriesNumber | integer | Home team series number. |
| teams.home.team.id | integer | Home team MLB ID. |
| teams.home.team.name | character | Home team name. |
| teams.home.team.link | character | MLB Stats API relative home team link. |
| teams.home.leagueRecord.wins | integer | Home team league-record wins. |
| teams.home.leagueRecord.losses | integer | Home team league-record losses. |
| teams.home.leagueRecord.ties | integer | Home team league-record ties. |
| teams.home.leagueRecord.pct | character | Home team winning percentage. |
| venue.id | integer | Venue ID. |
| venue.name | character | Venue name. |
| venue.link | character | MLB Stats API relative venue link. |
| content.link | character | MLB Stats API relative game content link. |

## Details

Level IDs:

The following IDs can be passed to the level_ids argument:

1 = MLB 11 = Triple-A 12 = Doubl-A 13 = Class A Advanced 14 = Class A 15
= Class A Short Season 5442 = Rookie Advanced 16 = Rookie 17 = Winter
League

## Examples

``` r
# \donttest{
  try(mlb_game_pks("2019-04-29"))
#> ── MLB Game Pks data from MLB.com ─────────────────── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 12:40:49 UTC
#> # A tibble: 9 × 57
#>   game_pk gameGuid     link  gameType season gameDate officialDate isTie
#>     <int> <chr>        <chr> <chr>    <chr>  <chr>    <chr>        <lgl>
#> 1  565909 f66a1949-a0… /api… R        2019   2019-04… 2019-04-29   FALSE
#> 2  566001 204d367a-81… /api… R        2019   2019-04… 2019-04-29   FALSE
#> 3  565040 455fea2b-f0… /api… R        2019   2019-04… 2019-04-29   FALSE
#> 4  567173 220d5ff9-d2… /api… R        2019   2019-04… 2019-04-29   FALSE
#> 5  566975 08410240-7e… /api… R        2019   2019-04… 2019-04-29   FALSE
#> 6  567566 2aeee258-c3… /api… R        2019   2019-04… 2019-04-29   FALSE
#> 7  567275 8bbe8497-80… /api… R        2019   2019-04… 2019-04-29   FALSE
#> 8  565717 62a6f6de-55… /api… R        2019   2019-04… 2019-04-29   FALSE
#> 9  566484 0680b3a3-16… /api… R        2019   2019-04… 2019-04-29   FALSE
#> # ℹ 49 more variables: gameNumber <int>, publicFacing <lgl>,
#> #   doubleHeader <chr>, gamedayType <chr>, tiebreaker <chr>,
#> #   calendarEventID <chr>, seasonDisplay <chr>, dayNight <chr>,
#> #   scheduledInnings <int>, reverseHomeAwayStatus <lgl>,
#> #   inningBreakLength <int>, gamesInSeries <int>,
#> #   seriesGameNumber <int>, seriesDescription <chr>,
#> #   recordSource <chr>, ifNecessary <chr>, …
# }
```

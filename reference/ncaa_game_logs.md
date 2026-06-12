# **Get NCAA Baseball Game Logs**

**Get NCAA Baseball Game Logs**

## Usage

``` r
ncaa_game_logs(player_id, year, type = "batting", span = "game", ...)
```

## Arguments

- player_id:

  A player's unique id. Can be found using the get_ncaa_baseball_roster
  function.

- year:

  The year of interest.

- type:

  The kind of statistics you want to return. Current options are
  'batting', 'pitching', or 'fielding'.

- span:

  The span of time; can either be 'game' for game logs in a season, or
  'career' which returns seasonal stats for a player's career.

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame containing player and school information as well as
game-by-game statistics. The exact stat columns vary by `type` (batting
/ pitching / fielding) and `span` (game / career); the table below shows
the pitching (`type = "pitching"`, `span = "game"`) columns.

|               |           |                                           |
|---------------|-----------|-------------------------------------------|
| col_name      | types     | description                               |
| player_id     | numeric   | stats.ncaa.org player identifier.         |
| player_name   | character | Player name.                              |
| Date          | character | Game date.                                |
| Opponent      | character | Opponent name.                            |
| Result        | character | Game result (W/L and score).              |
| App           | numeric   | Appearances.                              |
| G             | numeric   | Games.                                    |
| GS            | numeric   | Games started.                            |
| IP            | numeric   | Innings pitched.                          |
| CG            | numeric   | Complete games.                           |
| H             | numeric   | Hits allowed.                             |
| R             | numeric   | Runs allowed.                             |
| ER            | numeric   | Earned runs allowed.                      |
| BB            | numeric   | Walks (bases on balls) allowed.           |
| SO            | numeric   | Strikeouts.                               |
| SHO           | numeric   | Shutouts.                                 |
| BF            | numeric   | Batters faced.                            |
| P-OAB         | numeric   | Opponent at-bats.                         |
| 2B-A          | numeric   | Doubles allowed.                          |
| 3B-A          | numeric   | Triples allowed.                          |
| Bk            | numeric   | Balks.                                    |
| HR-A          | numeric   | Home runs allowed.                        |
| WP            | numeric   | Wild pitches.                             |
| HB            | numeric   | Hit batters.                              |
| IBB           | numeric   | Intentional walks allowed.                |
| Inh Run       | numeric   | Inherited runners.                        |
| Inh Run Score | numeric   | Inherited runners who scored.             |
| SHA           | numeric   | Sacrifice hits allowed.                   |
| SFA           | numeric   | Sacrifice flies allowed.                  |
| Pitches       | numeric   | Pitch count.                              |
| GO            | numeric   | Ground outs induced.                      |
| FO            | numeric   | Fly outs induced.                         |
| W             | numeric   | Wins.                                     |
| L             | numeric   | Losses.                                   |
| SV            | numeric   | Saves.                                    |
| OrdAppeared   | numeric   | Order in which the pitcher appeared.      |
| KL            | numeric   | Strikeouts looking (called third strike). |
| pickoffs      | character | Pickoffs.                                 |

## Details

Live usage (reads `stats.ncaa.org`, which is behind Akamai bot
protection and needs the optional `chromote` + Google Chrome browser
fallback, so it is shown here rather than as a runnable example):

    ncaa_game_logs(player_id = 2649785, year = 2023, type = "pitching", span = "game")
    ncaa_game_logs(player_id = 2477974, year = 2023, type = "pitching", span = "career")
    ncaa_game_logs(player_id = 2680961, year = 2023, type = "batting", span = "game")
    ncaa_game_logs(player_id = 2486588, year = 2023, type = "batting", span = "career")

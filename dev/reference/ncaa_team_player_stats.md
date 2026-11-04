# **Scrape NCAA baseball Team Player Stats (Division I, II, and III)**

This function allows the user to obtain batting, pitching, or fielding
statistics for any school affiliated with the NCAA at the division I,
II, or III levels. The function acquires data from the NCAA's website
(stats.ncaa.org) and returns a tibble.

## Usage

``` r
ncaa_team_player_stats(
  team_id,
  year = most_recent_ncaa_baseball_season(),
  type = "batting",
  ...
)
```

## Arguments

- team_id:

  The numerical ID that the NCAA website uses to identify a team

- year:

  The season for which data should be returned, in the form of "YYYY".
  Years currently available: 2013-2017.

- type:

  A string indicating whether to return "batting", "pitching", or
  "fielding" statistics

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame with the following variables. The exact stat columns vary
by `type` (batting / pitching / fielding); the table below shows the
batting (`type = "batting"`) columns.

|               |           |                                               |
|---------------|-----------|-----------------------------------------------|
| col_name      | types     | description                                   |
| year          | integer   | Season (4-digit year).                        |
| team_name     | character | Team name.                                    |
| team_id       | numeric   | Team NCAA id.                                 |
| conference_id | integer   | Conference identifier.                        |
| conference    | character | Conference name.                              |
| division      | numeric   | NCAA division (1, 2, 3).                      |
| player_id     | integer   | stats.ncaa.org player identifier.             |
| player_url    | character | Full stats.ncaa.org url for the player page.  |
| player_name   | character | Player name.                                  |
| Yr            | character | Academic class/year (Fr, So, Jr, Sr).         |
| Pos           | character | Primary fielding position.                    |
| Jersey        | character | Jersey number (the site's "#" column).        |
| GP            | numeric   | Games played.                                 |
| GS            | numeric   | Games started.                                |
| BA            | numeric   | Batting average.                              |
| OBPct         | numeric   | On-base percentage.                           |
| SlgPct        | numeric   | Slugging percentage.                          |
| R             | numeric   | Runs scored.                                  |
| AB            | numeric   | At-bats.                                      |
| H             | numeric   | Hits.                                         |
| 2B            | numeric   | Doubles.                                      |
| 3B            | numeric   | Triples.                                      |
| TB            | numeric   | Total bases.                                  |
| HR            | numeric   | Home runs.                                    |
| RBI           | numeric   | Runs batted in.                               |
| BB            | numeric   | Walks (bases on balls).                       |
| HBP           | numeric   | Hit by pitch.                                 |
| SF            | numeric   | Sacrifice flies.                              |
| SH            | numeric   | Sacrifice hits (bunts).                       |
| K             | numeric   | Strikeouts.                                   |
| DP            | numeric   | Grounded into double plays (site's "OPP DP"). |
| CS            | numeric   | Caught stealing.                              |
| Picked        | numeric   | Times picked off.                             |
| SB            | numeric   | Stolen bases.                                 |
| RBI2out       | numeric   | Runs batted in with two outs.                 |

## Details

Live usage (reads `stats.ncaa.org`, which is behind Akamai bot
protection and needs the optional `chromote` + Google Chrome browser
fallback, so it is shown here rather than as a runnable example):

    ncaa_team_player_stats(team_id = 234, year = 2023, type = "batting")

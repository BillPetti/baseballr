# **Scrape NCAA baseball Team Player Stats (Division I, II, and III)**

This function allows the user to obtain batting or pitching statistics
for any school affiliated with the NCAA at the division I, II, or III
levels. The function acquires data from the NCAA's website
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

  A string indicating whether to return "batting" or "pitching"
  statistics

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame with the following variables

|               |           |
|---------------|-----------|
| col_name      | types     |
| year          | integer   |
| team_name     | character |
| team_id       | numeric   |
| conference_id | integer   |
| conference    | character |
| division      | numeric   |
| player_id     | integer   |
| player_url    | character |
| player_name   | character |
| Yr            | character |
| Pos           | character |
| Jersey        | character |
| GP            | numeric   |
| GS            | numeric   |
| BA            | numeric   |
| OBPct         | numeric   |
| SlgPct        | numeric   |
| R             | numeric   |
| AB            | numeric   |
| H             | numeric   |
| 2B            | numeric   |
| 3B            | numeric   |
| TB            | numeric   |
| HR            | numeric   |
| RBI           | numeric   |
| BB            | numeric   |
| HBP           | numeric   |
| SF            | numeric   |
| SH            | numeric   |
| K             | numeric   |
| DP            | numeric   |
| CS            | numeric   |
| Picked        | numeric   |
| SB            | numeric   |
| RBI2out       | numeric   |

## Examples

``` r
# \donttest{
  try(ncaa_team_player_stats(team_id = 234, year = 2023, type = "batting"))
#> ! 2026-06-08 03:47:14.620458: stats.ncaa.org returned an Akamai bot-challenge for the team stats endpoint; no data could be retrieved. This endpoint is gated and cannot be scraped without a browser session.
#> data frame with 0 columns and 0 rows
# }
```

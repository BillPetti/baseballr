# **(legacy) Scrape NCAA baseball Team Player Stats (Division I, II, and III)**

**(legacy) Scrape NCAA baseball Team Player Stats (Division I, II, and
III)**

## Usage

``` r
ncaa_scrape(
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

# **(legacy) Query Statcast by Date Range and Players**

**(legacy) Query Statcast by Date Range and Players**

## Usage

``` r
scrape_statcast_savant(
  start_date = Sys.Date() - 1,
  end_date = Sys.Date(),
  playerid = NULL,
  player_type = "batter",
  ...
)

scrape_statcast_savant.Date(
  start_date = Sys.Date() - 1,
  end_date = Sys.Date(),
  playerid = NULL,
  player_type = "batter",
  ...
)

scrape_statcast_savant.default(
  start_date = Sys.Date() - 1,
  end_date = Sys.Date(),
  playerid = NULL,
  player_type = "batter",
  ...
)

scrape_statcast_savant_batter(start_date, end_date, batterid = NULL, ...)

scrape_statcast_savant_batter_all(start_date, end_date, batterid = NULL, ...)

scrape_statcast_savant_pitcher(start_date, end_date, pitcherid = NULL, ...)

scrape_statcast_savant_pitcher_all(start_date, end_date, pitcherid = NULL, ...)
```

## Arguments

- start_date:

  Date of first game for which you want data. Format must be in
  YYYY-MM-DD format.

- end_date:

  Date of last game for which you want data. Format must be in
  YYYY-MM-DD format.

- playerid:

  The MLBAM ID for the player whose data you want to query.

- player_type:

  The player type. Can be `batter` or `pitcher`. Default is `batter`

- ...:

  currently ignored

- batterid:

  The MLBAM ID for the batter whose data you want to query.

- pitcherid:

  The MLBAM ID for the pitcher whose data you want to query.

## Value

Returns a tibble with Statcast data.

Returns a tibble with Statcast data.

Returns a tibble with Statcast data.

Returns a tibble with Statcast data.

Returns a tibble with Statcast data.

Returns a tibble with Statcast data.

Returns a tibble with Statcast data.

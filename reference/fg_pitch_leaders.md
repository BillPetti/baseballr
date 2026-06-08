# **(legacy) Scrape Pitcher Leaderboards from FanGraphs**

**(legacy) Scrape Pitcher Leaderboards from FanGraphs**

## Usage

``` r
fg_pitch_leaders(
  age = "",
  pos = "all",
  stats = "pit",
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
  type = "8",
  postseason = "",
  sortdir = "default",
  sortstat = "WAR"
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

  (character) Sort by stat. Default is "WAR".

## Value

A data frame of pitcher data.

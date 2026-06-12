# **(legacy) Get Play-By-Play Data for NCAA Baseball Games**

**(legacy) Get Play-By-Play Data for NCAA Baseball Games**

**(legacy) Get Play-By-Play Data for NCAA Baseball Games**

## Usage

``` r
get_ncaa_baseball_pbp(
  game_info_url = NA_character_,
  game_pbp_url = NA_character_,
  raw_html_to_disk = FALSE,
  raw_html_path = "/",
  read_from_file = FALSE,
  file = NA_character_,
  ...
)

ncaa_baseball_pbp(
  game_info_url = NA_character_,
  game_pbp_url = NA_character_,
  raw_html_to_disk = FALSE,
  raw_html_path = "/",
  read_from_file = FALSE,
  file = NA_character_,
  ...
)
```

## Arguments

- game_info_url:

  The url for the game's boxscore data. This can be found using the
  ncaa_schedule_info function.

- game_pbp_url:

  The url for the game's play-by-play data. This can be found using the
  ncaa_schedule_info function.

- raw_html_to_disk:

  Write raw html to disk (saves as `{game_pbp_id}`.html in
  `raw_html_path` directory)

- raw_html_path:

  Directory path to write raw html

- read_from_file:

  Read from raw html on disk

- file:

  File with full path to read raw html

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame with play-by-play data for an individual game.

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_date | character | Game date (NA on the redesigned page; use [`ncaa_schedule_info()`](https://billpetti.github.io/baseballr/reference/ncaa_schedule_info.md)). |
| location | character | Venue / conditions line when present. |
| attendance | logical | Reported attendance (NA on the redesigned page). |
| inning | character | Inning number. |
| inning_top_bot | character | Half-inning ("top" or "bot"). |
| score | character | Running score (away-home) after the play. |
| batting | character | Batting team name. |
| fielding | character | Fielding team name. |
| description | character | Play description text. |
| game_pbp_url | character | stats.ncaa.org play-by-play url for the game. |
| game_pbp_id | integer | stats.ncaa.org play-by-play (contest) identifier. |

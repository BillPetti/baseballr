# **Get Play-By-Play Data for NCAA Baseball Games**

**Get Play-By-Play Data for NCAA Baseball Games**

## Usage

``` r
ncaa_pbp(
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

|                |           |
|----------------|-----------|
| col_name       | types     |
| game_date      | character |
| location       | character |
| attendance     | logical   |
| inning         | character |
| inning_top_bot | character |
| score          | character |
| batting        | character |
| fielding       | character |
| description    | character |
| game_pbp_url   | character |
| game_pbp_id    | integer   |

## Examples

``` r
# \donttest{
  try(ncaa_pbp(game_info_url = "https://stats.ncaa.org/contests/2167178/box_score"))
#> 
#> Error in ncaa_pbp(game_info_url = "https://stats.ncaa.org/contests/2167178/box_score") : 
#>   object 'mapped_table' not found
# }
```

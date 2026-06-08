# **Get Schedule and Results for NCAA Baseball Teams**

**Get Schedule and Results for NCAA Baseball Teams**

## Usage

``` r
ncaa_schedule_info(team_id = NULL, year = NULL, pbp_links = FALSE, ...)
```

## Arguments

- team_id:

  The team's unique NCAA id.

- year:

  The season (i.e. use 2016 for the 2015-2016 season, etc.)

- pbp_links:

  Logical parameter to run process for scraping play_by_play urls for
  each game

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame with the following fields: date, opponent, result, score,
innings (if more than regulation), and the url for the game itself.

|                         |           |
|-------------------------|-----------|
| col_name                | types     |
| year                    | integer   |
| season_id               | integer   |
| date                    | character |
| home_team               | character |
| home_team_id            | integer   |
| home_team_conference    | character |
| home_team_conference_id | integer   |
| home_team_slug          | character |
| home_team_division      | integer   |
| away_team               | character |
| away_team_id            | integer   |
| away_team_conference    | character |
| away_team_conference_id | integer   |
| away_team_slug          | character |
| away_team_division      | integer   |
| neutral_site            | character |
| result                  | character |
| score                   | character |
| innings                 | character |
| slug                    | character |
| game_info_url           | character |
| contest_id              | integer   |

## Details

     try(ncaa_schedule_info(team_id = 736, year = 2019))

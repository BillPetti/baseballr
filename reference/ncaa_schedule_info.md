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

|  |  |  |
|----|----|----|
| col_name | types | description |
| year | integer | Season (4-digit year). |
| season_id | integer | stats.ncaa.org season identifier. |
| date | character | Game date. |
| home_team | character | Home team name. |
| home_team_id | integer | Home team NCAA id. |
| home_team_conference | character | Home team conference name. |
| home_team_conference_id | integer | Home team conference id. |
| home_team_slug | character | Relative stats.ncaa.org url for the home team. |
| home_team_division | integer | Home team NCAA division (1, 2, 3). |
| away_team | character | Away team name. |
| away_team_id | integer | Away team NCAA id. |
| away_team_conference | character | Away team conference name. |
| away_team_conference_id | integer | Away team conference id. |
| away_team_slug | character | Relative stats.ncaa.org url for the away team. |
| away_team_division | integer | Away team NCAA division (1, 2, 3). |
| neutral_site | character | Neutral-site venue (when not hosted by either team). |
| result | character | Win/loss/tie result for `team_id`. |
| score | character | Final score (e.g. "7-3"). |
| innings | character | Innings played when other than regulation (extras). |
| slug | character | Relative stats.ncaa.org url for the game. |
| game_info_url | character | Full stats.ncaa.org box-score url for the game. |
| contest_id | integer | stats.ncaa.org contest (game) identifier. |

## Details

     try(ncaa_schedule_info(team_id = 736, year = 2019))

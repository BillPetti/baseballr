# **(legacy) Get Park Effects for NCAA Baseball Teams**

**(legacy) Get Park Effects for NCAA Baseball Teams**

## Usage

``` r
get_ncaa_park_factor(team_id, years, type = "conference", ...)
```

## Arguments

- team_id:

  The team's unique NCAA id.

- years:

  The season or seasons (i.e. use 2016 for the 2015-2016 season, etc.,
  limited to just 2013-2023 seasons).

- type:

  default is conference. the conference parameter adjusts for the
  conference the school plays in, the division parameter calculates
  based on the division the school plays in 1,2,or 3. Defaults to
  'conference'.

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame with the following fields: school, home_game, away_game,
runs_scored_home, runs_allowed_home, run_scored_away, runs_allowed_away,
base_pf (base park factor), home_game_adj (an adjustment for the
percentage of home games played) final_pf (park factor after
adjustments)

|                   |           |                                                |
|-------------------|-----------|------------------------------------------------|
| col_name          | types     | description                                    |
| school            | character | Team name.                                     |
| home_game         | numeric   | Number of home games in the sample.            |
| away_game         | numeric   | Number of away games in the sample.            |
| runs_scored_home  | numeric   | Runs scored by the team in home games.         |
| runs_allowed_home | numeric   | Runs allowed by the team in home games.        |
| runs_scored_away  | numeric   | Runs scored by the team in away games.         |
| runs_allowed_away | numeric   | Runs allowed by the team in away games.        |
| base_pf           | numeric   | Base park factor (before adjustment).          |
| home_game_adj     | numeric   | Adjustment for the share of home games played. |
| final_pf          | numeric   | Park factor after adjustments.                 |

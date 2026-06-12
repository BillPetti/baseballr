# **(legacy) Get NCAA Baseball Rosters**

**(legacy) Get NCAA Baseball Rosters**

**(legacy) Get NCAA Baseball Rosters**

## Usage

``` r
ncaa_baseball_roster(team_id = NULL, year, ...)

get_ncaa_baseball_roster(team_id = NULL, year, ...)
```

## Arguments

- team_id:

  NCAA id for a school

- year:

  The year of interest

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame containing roster information, including IDs and urls for
each player (if available)

|               |           |                                              |
|---------------|-----------|----------------------------------------------|
| col_name      | types     | description                                  |
| player_name   | character | Player name.                                 |
| class         | character | Academic class/year (Fr, So, Jr, Sr).        |
| player_id     | character | stats.ncaa.org player identifier.            |
| season        | numeric   | Season (4-digit year).                       |
| number        | character | Jersey number.                               |
| position      | character | Primary fielding position.                   |
| player_url    | character | Full stats.ncaa.org url for the player page. |
| team_name     | character | Team name.                                   |
| conference    | character | Conference name.                             |
| team_id       | numeric   | Team NCAA id.                                |
| division      | numeric   | NCAA division (1, 2, 3).                     |
| conference_id | numeric   | Conference identifier.                       |

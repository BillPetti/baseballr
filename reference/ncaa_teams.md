# **Scrape NCAA baseball Teams (Division I, II, and III)**

This function allows the user to obtain NCAA teams by year and division

## Usage

``` r
ncaa_teams(year = most_recent_ncaa_baseball_season(), division = 1, ...)
```

## Arguments

- year:

  The season for which data should be returned, in the form of "YYYY".
  Years currently available: 2002 onward.

- division:

  Division - 1, 2, 3

- ...:

  Additional arguments passed to an underlying function like httr.

## Value

A data frame with the following variables

|                |           |                                                   |
|----------------|-----------|---------------------------------------------------|
| col_name       | types     | description                                       |
| team_id        | character | Franchise team id (legacy `/team/{id}/...` urls). |
| team_name      | character | Team display name.                                |
| team_url       | character | Relative team url from stats.ncaa.org.            |
| conference_id  | character | Conference identifier.                            |
| conference     | character | Conference name.                                  |
| division       | numeric   | NCAA division (1, 2, 3).                          |
| year           | numeric   | Season (4-digit year).                            |
| season_id      | character | Season id (legacy urls); join key to season ids.  |
| season_team_id | character | Season-team id (modern `/teams/{id}` urls).       |

## Details

    ncaa_teams(2023, 1)

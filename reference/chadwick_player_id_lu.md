# **Look up Baseball Player IDs by Player Name**

This function allows you to query the Chadwick Bureau's public register
of baseball players and the various IDs associated with them in
different systems of record.

## Usage

``` r
playerid_lookup(last_name = NULL, first_name = NULL)
```

## Arguments

- last_name:

  A text string used to return results for players with that string in
  their last name.

- first_name:

  A text string used to return results for players with that string in
  their first name.

## Value

A data frame of baseball players and the various IDs associated with
them in different systems of record.

|                  |           |                                       |
|------------------|-----------|---------------------------------------|
| col_name         | types     | description                           |
| first_name       | character | Player first name.                    |
| last_name        | character | Player last name.                     |
| given_name       | character | Player full given (legal) name.       |
| name_suffix      | character | Name suffix (e.g. Jr., Sr., III).     |
| nick_name        | character | Player nickname.                      |
| birth_year       | integer   | Year of birth.                        |
| mlb_played_first | integer   | First MLB season as a player.         |
| mlbam_id         | integer   | MLB Advanced Media (MLBAM) player ID. |
| retrosheet_id    | character | Retrosheet player ID.                 |
| bbref_id         | character | Baseball-Reference player ID.         |
| fangraphs_id     | integer   | FanGraphs player ID.                  |

## Examples

``` r
# \donttest{
  try(playerid_lookup("Garcia", "Karim"))
#> ── Player ID Lookup from the Chadwick Bureau's public register of baseba
#> ℹ Data updated: 2026-06-08 03:18:02 UTC
#> # A tibble: 0 × 11
#> # ℹ 11 variables: first_name <chr>, last_name <chr>, given_name <chr>,
#> #   name_suffix <chr>, nick_name <chr>, birth_year <int>,
#> #   mlb_played_first <int>, mlbam_id <int>, retrosheet_id <chr>,
#> #   bbref_id <chr>, fangraphs_id <int>
# }
```

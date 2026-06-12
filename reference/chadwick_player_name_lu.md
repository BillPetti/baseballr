# **Look up Baseball Player Name by ID**

This function allows you to query the Chadwick Bureau's public register
of baseball players and the various IDs associated with them in
different systems of record.

## Usage

``` r
playername_lookup(id)
```

## Arguments

- id:

  An integer or character string representing a player ID in a baseball
  database, cross-referenced through the Chadwick Bureau's public
  register of baseball players.

## Value

A data frame of baseball players and the various IDs associated with
them in different systems of record.

|                  |           |                                       |
|------------------|-----------|---------------------------------------|
| col_name         | types     | description                           |
| name_first       | character | Player first name.                    |
| name_last        | character | Player last name.                     |
| name_given       | character | Player full given (legal) name.       |
| name_suffix      | character | Name suffix (e.g. Jr., Sr., III).     |
| name_nick        | character | Player nickname.                      |
| birth_year       | integer   | Year of birth.                        |
| mlb_played_first | integer   | First MLB season as a player.         |
| key_mlbam        | integer   | MLB Advanced Media (MLBAM) player ID. |
| key_retro        | character | Retrosheet player ID.                 |
| key_bbref        | character | Baseball-Reference player ID.         |
| key_fangraphs    | integer   | FanGraphs player ID.                  |

## Examples

``` r
# \donttest{
  try(playername_lookup(4885))
#> ── Player Name Lookup from the Chadwick Bureau's public register of base
#> ℹ Data updated: 2026-06-12 14:07:37 UTC
#> # A tibble: 1 × 11
#>   name_first name_last name_given  name_suffix name_nick  birth_year
#>   <chr>      <chr>     <chr>       <chr>       <chr>           <int>
#> 1 Nyjer      Morgan    Nyjer Jamid ""          Tony Plush       1980
#> # ℹ 5 more variables: mlb_played_first <int>, key_mlbam <int>,
#> #   key_retro <chr>, key_bbref <chr>, key_fangraphs <int>
  try(playername_lookup("kaaihki01"))
#> ── Player Name Lookup from the Chadwick Bureau's public register of base
#> ℹ Data updated: 2026-06-12 14:07:43 UTC
#> # A tibble: 1 × 11
#>   name_first name_last name_given name_suffix name_nick birth_year
#>   <chr>      <chr>     <chr>      <chr>       <chr>          <int>
#> 1 Kila       Ka'aihue  Micah Kila ""          ""              1984
#> # ℹ 5 more variables: mlb_played_first <int>, key_mlbam <int>,
#> #   key_retro <chr>, key_bbref <chr>, key_fangraphs <int>
# }
```

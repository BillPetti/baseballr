# **(legacy) Acquire pitch-by-pitch data for Major and Minor League games**

**(legacy) Acquire pitch-by-pitch data for Major and Minor League
games**

**(legacy) Acquire pitch-by-pitch data for Major and Minor League
games**

## Usage

``` r
get_pbp_mlb(game_pk, add_base_state = FALSE)

get_pbp_mlb(game_pk, add_base_state = FALSE)
```

## Arguments

- game_pk:

  The date for which you want to find game_pk values for MLB games

- add_base_state:

  If `TRUE`, append per-event pre-pitch base-occupancy columns
  `pre_on_1b` / `pre_on_2b` / `pre_on_3b` (runner MLBAM ids, `NA` when
  the base is empty), reconstructed from the feed's runner-movement
  records. The API itself only publishes end-of-plate-appearance base
  state (`matchup.postOn*`); this derives the state before each pitch.
  Defaults to `FALSE`.

## Value

Returns a tibble that includes over 100 columns of data provided by the
MLB Stats API at a pitch level.

Returns a tibble that includes over 100 columns of data provided by the
MLB Stats API at a pitch level.

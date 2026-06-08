# **Create stat lines from Statcast data**

This function allows you to create stat lines of statistics for players
or groups of players from raw Statcast. When calculating wOBA, the most
recent year in the data frame is used for weighting.

## Usage

``` r
statline_from_statcast(df, base = "pa")
```

## Arguments

- df:

  A data frame of statistics that includes, at a minimum, the following
  columns: events, description, game_date, and type.

- base:

  Tells the function what to use as the population of pitches to use for
  the stat line. Options include "swings", "contact", or "pa". Defaults
  to "pa".

## Value

A tibble with the additional columns calculated using the Statcast data.

## Details

      statline_from_statcast(df, base = "contact")

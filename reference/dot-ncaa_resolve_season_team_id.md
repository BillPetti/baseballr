# **Resolve a stats.ncaa.org season-team id**

`stats.ncaa.org` migrated team statistics from the franchise-centric
`/team/{team_id}/stats?...` query form to a season-team resource at
`/teams/{season_team_id}/season_to_date_stats`. The `season_team_id` is
year-specific and is **not** the franchise `team_id`. This helper
resolves it by reading the still-working roster page
(`/team/{team_id}/roster/{season_id}`) and extracting the "Team
Statistics" link it points to.

## Usage

``` r
.ncaa_resolve_season_team_id(team_id, season_id, ...)
```

## Arguments

- team_id:

  Franchise team id used by the NCAA site.

- season_id:

  Season id from
  [`load_ncaa_baseball_season_ids()`](https://billpetti.github.io/baseballr/reference/load_ncaa_baseball_season_ids.md)
  (the `id` column), i.e. the value the roster URL expects.

- ...:

  Passed through to
  [`request_with_proxy()`](https://billpetti.github.io/baseballr/reference/request_with_proxy.md)
  (e.g. `proxy`).

## Value

A length-1 character season-team id, or `NA_character_` if the roster
page could not be read (e.g. challenged) or the link was not found.

## Release summary

This release adds a new data source, modernizes the HTTP layer, and fixes a
number of upstream-driven breakages.

* **New ESPN MLB data source.** Adds a full family of ESPN Major League Baseball
  wrappers (`espn_mlb_*()`, ~107 functions) mirroring the naming convention used
  across the SportsDataverse packages. Coverage includes scoreboard / schedule,
  play-by-play, team and player box scores, game rosters, standings, betting,
  athletes, coaches, seasons, franchises, drafts, leaders, and league/event
  reference data, plus baseball-specific extractors for probable starting
  pitchers (`espn_mlb_game_probables()`) and venue / attendance / umpire crew
  (`espn_mlb_game_info()`). Every data-returning function documents its output
  with an `@return` column table.

* **HTTP layer migrated from `httr` to `httr2`.** `httr` is dropped and
  `httr2 (>= 1.0.0)` is added. `Depends: R (>= 4.1.0)` (the package now uses the
  native pipe `|>` throughout).

* **Upstream fixes.** FanGraphs requests now pass Cloudflare's challenge (the
  `fg_*()` leaders/team/game-log functions and the `.aspx` scrapers return data
  again); the NCAA wrappers (`ncaa_*()`) work again behind the site's Akamai
  bot-protection; `statcast_search()` assigns Baseball Savant columns
  length-tolerantly; and the Spotrac payroll functions track the site's new URLs
  and schema.

Examples that contact remote services are wrapped in `\donttest{}` and fail
gracefully. Vignettes are included for getting started, NCAA scraping, and
Statcast usage.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and
dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

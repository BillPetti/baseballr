## Release summary

This release adds three new data-source families, modernizes the HTTP layer, and
fixes a number of upstream-driven breakages.

* **New ESPN MLB data source.** Adds a full family of ESPN Major League Baseball
  wrappers (`espn_mlb_*()`, 107 functions) mirroring the naming convention used
  across the SportsDataverse packages. Coverage includes scoreboard / schedule,
  play-by-play, team and player box scores, game rosters, standings, betting,
  athletes, coaches, seasons, franchises, drafts, leaders, and league/event
  reference data, plus baseball-specific extractors for probable starting
  pitchers (`espn_mlb_game_probables()`) and venue / attendance / umpire crew
  (`espn_mlb_game_info()`). Every data-returning function documents its output
  with an `@return` column table.

* **New ESPN College Baseball data source.** Adds a full family of ESPN NCAA
  college-baseball wrappers (`espn_college_baseball_*()`, 70 functions) -- thin
  league-parameterized twins of the ESPN MLB family -- covering scoreboard,
  play-by-play, team and player box scores, rosters, teams, standings, rankings,
  conferences, athletes, leaders, tournaments (the College World Series), and the
  core reference graph.

* **New Fox Sports (Bifrost) MLB data source.** Adds read-only Fox Sports MLB
  wrappers (`fox_mlb_*()`, 6 functions: team roster, team stats, team game log,
  standings, league leaders, and odds) over `api.foxsports.com/bifrost/v1/mlb/*`.

* **HTTP layer migrated from `httr` to `httr2`.** `httr` is dropped and
  `httr2 (>= 1.0.0)` is added. `Depends: R (>= 4.1.0)` (the package now uses the
  native pipe `|>` throughout).

* **Upstream fixes.** FanGraphs requests now pass Cloudflare's challenge (the
  `fg_*()` leaders/team/game-log functions and the `.aspx` scrapers return data
  again); the NCAA wrappers (`ncaa_*()`) can fetch `stats.ncaa.org` again after
  the site simultaneously tightened its Akamai bot-protection (a hard 403 or a
  soft HTTP-200 `bm-verify` interstitial) and redesigned its pages --
  `request_with_proxy()` now falls back to a stealth headless-Chrome fetch via
  the optional `chromote` package (added to `Suggests`; when absent the scrapers
  emit a clear install message) and each NCAA parser was updated for the new
  layout; `statcast_search()` assigns Baseball Savant columns length-tolerantly;
  and the Spotrac payroll functions track the site's new URLs and schema.

Examples that contact remote services are wrapped in `\donttest{}` and fail
gracefully; the NCAA examples that require the optional browser fallback are
shown in each function's documentation `Details` rather than as executable
examples, so routine checks never launch a browser. Vignettes are included for
getting started, NCAA scraping, and Statcast usage.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and
dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

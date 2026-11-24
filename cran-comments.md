## Release summary

This is a minor release, 2.0.0 -> 2.1.0. No exported function is removed or
renamed.

* **Fixes the R-devel NOTE** on the CRAN check page, "Base package in
  Suggests/Enhances imported in NAMESPACE: 'stats'": `stats` moves from
  `Suggests` to `Imports`.

* **New loaders (9).** `load_mlb_groups()`, `load_mlb_group_seasons()`,
  `load_mlb_group_aliases()`, `load_mlb_team_group_seasons()` and their four
  `load_ncaa_baseball_*()` twins (season-by-season league, division and
  conference memberships), and `load_mlb_park_dimensions()` (MLB park
  dimensions by season). All read public GitHub release assets.

* **Bug fixes.**
  - Wrappers that handled warnings with `tryCatch(warning = )` returned an
    empty result whenever a harmless warning was raised while parsing. The 124
    affected calls (ESPN wrappers, `bref_daily_batter()`, `fg_projections()`)
    now report the warning and keep the result.
  - `statcast_search_minors()` returned MLB pitches instead of minor-league
    pitches; it now sends `minors=true`.
  - `mlb_standings()` accepts a vector `league_id` as documented.
  - ESPN game-team roster, team leaders and conferences parsers updated for
    ESPN's current responses; `edge_code()` accepts Statcast columns.
  - Release-dataset loaders and `chadwick_player_lu()` raise the download
    timeout to at least 150 s and retry a failed download twice; a file that
    still fails is named in a warning.
  - `tail()` works on returned tibbles: their class vector no longer lists
    `data.table`.

* **Dependencies.** `httr2 (>= 1.1.0)` (was `>= 1.0.0`; `url_modify_query()`
  first appeared in 1.1.0). `Rcpp` and `RcppParallel` are dropped from
  `Imports` (the package has no compiled code and used neither). The unused
  `furrr`, `ggrepel`, `RSQLite`, `stringi` and `usethis` are dropped from
  `Suggests`.

Examples that contact remote services are wrapped in `\donttest{}` and fail
gracefully. The NCAA examples need an optional headless-browser fallback, so
they are shown in each function's `Details` rather than run. Tests that contact
remote services, and the offline tests that replay recorded responses, skip on
CRAN; the only tests that run on CRAN check the download-retry logic against
mocked reads and use no network.

## Test environments

* local: Windows 10 x64 (build 19045), x86_64-w64-mingw32, R 4.6.0
  (2026-04-24 ucrt): `R CMD build` then `R CMD check --as-cran` on the tarball
* win-builder: R-devel and R-release, submitted 2026-11-24, results pending
* R-hub v2: linux, windows, macos (R-devel), dispatched 2026-11-24, results
  pending
* GitHub Actions R-CMD-check: macos-latest (release), windows-latest
  (release), ubuntu-latest (devel, release, oldrel-1), on the release pull
  request

## R CMD check results

Local `R CMD check --as-cran` (including `--run-donttest` examples and the
PDF/HTML manuals):

0 errors | 0 warnings | 0 notes

## revdepcheck results

There are no reverse dependencies on CRAN
(`tools::package_dependencies("baseballr", reverse = TRUE, which = "all")`,
2026-11-24).

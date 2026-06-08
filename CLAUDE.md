<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [CLAUDE.md -- baseballr Development Guide](#claudemd----baseballr-development-guide)
- [Package Overview](#package-overview)
- [Branching & PR Workflow](#branching--pr-workflow)
- [Build & Development Commands](#build--development-commands)
- [Project Structure](#project-structure)
- [Key Coding Conventions](#key-coding-conventions)
- [Testing](#testing)
- [Baseball-Specific Details](#baseball-specific-details)
- [NAMESPACE](#namespace)
- [Documentation Maintenance](#documentation-maintenance)
- [Commit Convention](#commit-convention)
- [Common Pitfalls](#common-pitfalls)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CLAUDE.md -- baseballr Development Guide

## Package Overview

baseballr is an R package for acquiring and analyzing baseball data from online
sources. It wraps the MLB Stats API, ESPN (MLB), FanGraphs, Baseball Reference,
Baseball Savant / Statcast, the NCAA baseball stats site, Spotrac, the Chadwick
Bureau register, and Retrosheet, exporting ~290 functions across multiple
data-source families: `mlb_*()`, `espn_mlb_*()`, `fg_*()`, `bref_*()`,
`statcast_*()` / `scrape_statcast_savant*()`, `ncaa_*()`, `sptrc_*()`,
`chadwick_*()`, `retrosheet_*()`, plus `load_*()` data loaders,
`metrics`/`woba_plus`/`fip_plus` analytics, and the `ggspraychart()`
visualization.

When this guide differs from current repository docs, treat `CONTRIBUTING.md` and
current test implementations as authoritative.

- **Version**: 2.0.0
- **R Requirement**: >= 4.1.0 (the package uses the native pipe `|>`)
- **License**: MIT
- **Author (aut)**: Bill Petti <billpetti@gmail.com>
- **Maintainer (cre)**: Saiem Gilani <saiem.gilani@gmail.com>
- **pkgdown site**: https://billpetti.github.io/baseballr/
- **Repo**: BillPetti/baseballr
- **Branch**: `master` is the default branch and release branch.

## Branching & PR Workflow

- `master` is the default branch and release branch (NOT `main`).
- Active development happens on `development_branch`; create feature branches
  from the latest stable base and target `master` in PRs.
- Keep code, tests, and roxygen/docs updates in the same PR when changing
  exported behavior.

## Build & Development Commands

```r
# Regenerate roxygen documentation + NAMESPACE
devtools::document()

# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-mlb_pbp.R")

# Full R CMD check
devtools::check()

# Install locally
devtools::install()

# Build pkgdown site locally
pkgdown::build_site()
```

## Project Structure

```
R/                            # All package source code
  mlb.R                       # MLB Stats API doc-grouping topic + shared helpers
  mlb_*.R                     # MLB Stats API endpoint wrappers (~110 functions):
                              #   types/codes, game, players, teams, standings,
                              #   league, draft, awards, jobs, all-star, hr derby
  fangraphs.R                 # FanGraphs doc-grouping topic
  fg_*.R                      # FanGraphs wrappers: fg_batter_leaders, fg_pitcher_leaders,
                              #   fg_fielder_leaders, fg_team_*, fg_milb_*, fg_park, fg_guts
  bref.R / bref_*.R           # Baseball Reference: bref_daily_batter/pitcher,
                              #   bref_standings_on_date, bref_team_results
  statcast / savant           # Baseball Savant: statcast_search(), statcast_leaderboards(),
  (in fangraphs/metrics +     #   scrape_statcast_savant*(), plus helpers code_barrel(),
   dedicated files)           #   edge_code(), process_statcast_payload(),
                              #   statline_from_statcast(), label_statcast_imputed_data()
  ncaa*.R                     # NCAA baseball: ncaa_scrape(), ncaa_pbp(), ncaa_roster(),
                              #   ncaa_schedule_info(), ncaa_game_logs(), ncaa_lineups(),
                              #   ncaa_park_factor(), ncaa_school_id_lu(), ncaa_teams()
  chadwick*.R                 # Chadwick Bureau register: chadwick_player_lu(),
                              #   playerid_lookup(), playername_lookup(), install helpers
  metrics.R / metrics_*.R     # woba_plus(), fip_plus(), team_consistency(),
                              #   run_expectancy_code(), linear_weights_savant()
  ggspraychart.R              # Spray-chart visualization
  load_*.R                    # Data-repository loaders (load_ncaa_baseball_*, etc.)
  utils.R                     # make_baseballr_data(), request_with_proxy(),
                              #   csv_from_url(), rds_from_url(), progressively()
  data.R                      # Documentation for bundled datasets
tests/testthat/               # Test files
man/                          # Auto-generated roxygen docs (DO NOT EDIT)
NAMESPACE                     # Auto-generated by roxygen2 (DO NOT EDIT)
```

## Key Coding Conventions

### Function Naming

| Data Source            | Prefix              | Example                                          |
| ---------------------- | ------------------- | ------------------------------------------------ |
| MLB Stats API          | `mlb_`              | `mlb_pbp()`, `mlb_game_linescore()`, `mlb_draft()` |
| FanGraphs              | `fg_`               | `fg_batter_leaders()`, `fg_team_pitcher()`       |
| Baseball Reference     | `bref_`             | `bref_daily_batter()`, `bref_team_results()`     |
| Baseball Savant        | `statcast_` / `sc_` | `statcast_search()`, `statcast_leaderboards()`   |
| ESPN MLB               | `espn_mlb_`         | `espn_mlb_pbp()`, `espn_mlb_scoreboard()`, `espn_mlb_team_box()` |
| NCAA baseball          | `ncaa_`             | `ncaa_scrape()`, `ncaa_roster()`                 |
| Spotrac                | `sptrc_`            | `sptrc_team_active_payroll()`                    |
| Chadwick Bureau        | `chadwick_`         | `chadwick_player_lu()`, `playerid_lookup()`      |
| Retrosheet             | `retrosheet_`       | `retrosheet_data()`                              |
| Analytics / metrics    | (named)             | `woba_plus()`, `fip_plus()`, `run_expectancy_code()` |
| Data loaders           | `load_`             | `load_ncaa_baseball_pbp()`, `load_umpire_ids()`  |
| Visualization          | (named)             | `ggspraychart()`                                 |

### Native Pipe

Use the **native pipe `|>`** everywhere. `magrittr` (`%>%`) is **retained as a
dependency only** because the NCAA wrappers still use it -- do not introduce new
`%>%` usage in non-NCAA code, and prefer migrating NCAA code to `|>` opportunistically.

### Data Processing Pipeline

```r
raw_data |>
  data.frame(stringsAsFactors = FALSE) |>
  dplyr::as_tibble() |>
  janitor::clean_names() |>
  make_baseballr_data("Description from source", Sys.time())
```

`make_baseballr_data()` (in `R/utils.R`) coerces to a tibble, sets the class to
`c("baseballr_data", "tbl_df", "tbl", "data.table", "data.frame")`, and attaches
`baseballr_timestamp` and `baseballr_type` attributes.

### Return-Value Initialization (CRITICAL)

Every wrapper that returns a variable assigned inside a `tryCatch` must initialize
that variable (usually `<- NULL`) **before** the `tryCatch` block. Otherwise, when
the upstream site errors (500s, timeouts, connection resets, IP bans) the `error`
handler runs, the return variable is never bound, and `return(<var>)` throws
`object '<var>' not found` instead of the intended `cli::cli_alert_danger()` +
empty fallback.

```r
mlb_func <- function(...) {
  url <- "https://statsapi.mlb.com/..."

  out <- NULL    # <-- MANDATORY. Not inside tryCatch.

  tryCatch(
    expr = {
      resp <- request_with_proxy(url, ...)
      out  <- resp |>
        # ...parse... |>
        janitor::clean_names() |>
        make_baseballr_data("Data from MLB.com", Sys.time())
    },
    error = function(e) {
      cli::cli_alert_danger("{Sys.time()}: Invalid arguments or no data available!")
      cli::cli_alert_danger("Error: {conditionMessage(e)}")
    },
    warning = function(w) {
      cli::cli_alert_warning("{Sys.time()}: Warning: {conditionMessage(w)}")
    },
    finally = {}
  )
  return(out)
}
```

This rule applies to **every return variable name** -- `out`, `df`, `pbp`,
`leaders`, `payrolls`, `roster`, `resp`, `data`, etc. Initialize to the
appropriate empty value: `NULL` for a single-object return, `data.frame()` /
`list()` where that is the documented shape.

### Messaging Layer

All user-facing messages use `cli`:
- `cli::cli_alert_danger()` in error handlers
- `cli::cli_alert_warning()` for warnings
- `cli::cli_alert_info()` for informational notes
- `cli::cli_warn()` / `cli::cli_abort()` for raised conditions

**Never pass a raw condition object to a `cli_*` function** -- the message string
is glue-interpolated, and a condition object will not render correctly (and can
leak `glue` syntax from the message). Pass `conditionMessage(cond)` through a value
placeholder instead:

```r
# WRONG
error = function(e) cli::cli_alert_danger("Error: {e}")

# RIGHT
error = function(e) cli::cli_alert_danger("Error: {conditionMessage(e)}")
```

### Column Drift Resilience

The upstream sites (MLB Stats API, FanGraphs, Baseball Reference, Savant) add,
rename, and occasionally drop columns over time. When dropping a known-transient
column, use `dplyr::select(-dplyr::any_of("colname"))` instead of
`dplyr::select(-"colname")`. The bare form errors the moment upstream removes that
column; `any_of` no-ops silently. The same `any_of` guard applies to renames:
`dplyr::rename(dplyr::any_of(c(new = "old")))`.

### Null Safety

Use `%||%` for null-safe defaults when parsing variable-shaped JSON:

```r
value <- obj$field %||% NA_character_
```

`data.frame(list_of_items)` can produce unexpected column types when the API
returns mixed `NULL`/value fields -- supply `%||%` defaults before binding.

### HTTP Layer

baseballr talks to several different hosts (statsapi.mlb.com, fangraphs.com,
baseball-reference.com, baseballsavant.mlb.com, the NCAA stats site, spotrac.com,
and ESPN's site.api / sports.core.api / site.web.api hosts), so there is no
single base-URL or proxy convention as in single-source packages. Requests go
through the package's own helpers in `R/utils.R`, `R/utils_mlb_stats.R`, and
`R/utils_espn.R` (`mlb_api_call()`, `fg_api_call()`, `request_with_proxy()`,
`.retry_request()`, `csv_from_url()`, `rds_from_url()`); build each wrapper on the
helper appropriate to its source rather than calling `httr`/`httr2` directly.

**ESPN MLB wrappers** (`espn_mlb_*()`) live in `R/espn_mlb_*.R` (public) over
shared internals in `R/espn_baseball_*_helpers.R` and the box parsers in
`R/espn_mlb_box_helpers.R`. They share an `httr2` layer in `R/utils_espn.R`
(`.retry_request()`, `.resp_text()`, `.report_api_error()` /
`.report_api_warning()`, `.empty_baseballr_data()`). `.retry_request()` honours a
proxy from `options(baseballr.proxy = ...)` (ESPN wrappers do not thread `...`,
so there is no per-call proxy override). Use `.report_api_error(e, hint, args)` in
ESPN error handlers (with `.args <- mget(setdiff(names(formals()), "..."))`), not
raw `cli::cli_alert_danger()`. ESPN's baseball game-summary boxscore groups stats
by side (`batting`/`pitching`/`fielding`), which is why the box parsers are
baseball-specific rather than shared with the basketball siblings.

**FanGraphs requires a Cloudflare-exempt `User-Agent`.** As of 2026-06-03
FanGraphs sits behind a Cloudflare JS challenge that 403s (`cf-mitigated:
challenge`) *every* unrecognized client -- including the plain/library
`User-Agent` that used to slip through -- and no header/TLS tweak passes it (it
needs a JS runtime). The challenge exempts the okhttp client the FanGraphs mobile
app uses, so **FanGraphs requests go through `fg_api_call()`**, which sends
`User-Agent: okhttp/4.12.0` (and emits a `cli` hint if a 403 still slips
through). `mlb_api_call()` keeps a plain package `User-Agent` for the MLB Stats
API (which ignores it). The `.aspx` scrapers (`fg_guts()`, `fg_park()`,
`fg_park_hand()`) and the FanGraphs game-log functions send the same okhttp UA
inline. Build new FanGraphs wrappers on `fg_api_call()`, never `mlb_api_call()`.
(The okhttp-UA approach was adapted from upstream PR #405; see `NEWS.md`.)

## Testing

Tests live in `tests/testthat/`. Many hit live sites and are gated / skipped
(e.g. `skip_on_cran()`, `skip_on_ci()`); do not assume they run unconditionally.

> **IMPORTANT CAUTION -- NCAA IP bans.** The NCAA stats site aggressively
> IP-bans scrapers. Tests and dev work that hit NCAA endpoints (`ncaa_*()`,
> `load_ncaa_baseball_*()`) must be done **sparingly and cached** -- prefer
> running them once and reusing fixtures, never in a tight loop, and keep them
> skipped on CI.

### Environment-variable gates

Every test that hits an external service is gated behind a source-specific
helper in `tests/testthat/helper-skip.R`. Each helper `skip()`s unless its
environment variable is set to `"1"`, so `R CMD check` (and CI) never fails when
an upstream site is down, rate-limiting, has changed its schema, or is blocking
automated access. **The first line of every live `test_that()` block is its
source gate.**

| Source                  | Env var                  | Helper                  |
| ----------------------- | ------------------------ | ----------------------- |
| FanGraphs (`fg_*`)      | `FANGRAPHS_TESTS=1`      | `skip_fangraphs_test()` |
| MLB Stats API (`mlb_*`) | `MLB_STATS_TESTS=1`      | `skip_mlb_test()`       |
| Baseball Reference      | `BREF_TESTS=1`           | `skip_bref_test()`      |
| Baseball Savant         | `STATCAST_TESTS=1`       | `skip_statcast_test()`  |
| NCAA (`ncaa_*`)         | `NCAA_BASEBALL_TESTS=1`           | `skip_ncaa_test()`      |
| Spotrac (`sptrc_*`)     | `SPOTRAC_TESTS=1`        | `skip_sptrc_test()`     |
| Chadwick (`chadwick_*`) | `CHADWICK_TESTS=1`       | `skip_chadwick_test()`  |
| Data loaders (`load_*`) | `BASEBALLR_LOAD_TESTS=1` | `skip_load_test()`      |
| ESPN MLB (`espn_mlb_*`) | `ESPN_MLB_TESTS=1`       | `skip_espn_test()`      |

```r
# Run one source's live tests locally:
Sys.setenv(MLB_STATS_TESTS = "1"); devtools::test()
```

Set `NCAA_BASEBALL_TESTS=1` only deliberately and sparingly (the NCAA site IP-bans
scrapers). Purely computational tests (e.g. `metrics_*` on a supplied data
frame) are **not** gated.

### Test Pattern

**Always use the subset direction for column assertions.** Because the upstream
sites add columns, strict `expect_equal` will break on any new column. The rule
is: the *expected* list must be a subset of the *actual* columns.

```r
test_that("mlb_function returns expected columns", {
  skip_mlb_test()          # source gate -- always the first line
  skip_on_cran()

  x <- mlb_function(game_pk = 632970)

  # Skip-if-empty guard -- right after the API call, before any column asserts.
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from endpoint at test time")
  }

  cols <- c("col1", "col2", "col3")
  expect_in(sort(cols), sort(colnames(x)))   # expected ⊆ actual
  expect_s3_class(x, "data.frame")
})
```

**Anti-patterns to avoid**:

```r
# WRONG - flags when upstream adds a column, even though it's non-breaking
expect_equal(sort(colnames(x)), sort(cols))

# WRONG - same direction problem, just phrased with expect_in
expect_in(sort(colnames(x)), sort(cols))
```

For dynamic columns, `expect_true(all(core_cols %in% colnames(x)))` is equivalent
to the subset-direction `expect_in()`.

**Per-element null checks**: if a function returns a named list of frames and the
source sometimes returns fewer than expected, use an inline helper so individual
asserts skip gracefully rather than erroring:

```r
check_cols <- function(component, cols) {
  if (is.null(component) || !is.data.frame(component) || ncol(component) == 0) {
    return(invisible(NULL))
  }
  expect_in(sort(cols), sort(colnames(component)))
  expect_s3_class(component, "data.frame")
}
```

## Baseball-Specific Details

- A regulation game is 9 innings; extra innings resolve ties. Tests and parsers
  must not assume a fixed inning count.
- MLB game identifiers are `game_pk` values (e.g. `632970`), used throughout the
  `mlb_*()` family.
- The MLB Stats API base host is `https://statsapi.mlb.com/`.
- Player identity is cross-source: the Chadwick Bureau register
  (`chadwick_player_lu()`, `playerid_lookup()`, `playername_lookup()`) maps
  between MLBAM, FanGraphs, Baseball Reference, and Retrosheet IDs -- prefer it
  when joining data across families.
- There is **no V2/V3 dichotomy** as in NBA/WNBA stats packages; each source has
  its own native payload shape.

## NAMESPACE

Auto-generated by roxygen2. **Never edit manually.** Run `devtools::document()` to
regenerate. The same applies to everything under `man/` -- never hand-edit `.Rd`
files.

## Documentation Maintenance

Regeneration steps are part of the commit workflow whenever the relevant sources
change. All are mechanical -- never edit the generated regions by hand.

### Markdown TOCs (doctoc)

`NEWS.md`, `CLAUDE.md`, `CONTRIBUTING.md`, `.github/copilot-instructions.md`, and
`.github/pull_request_template.md` carry a doctoc-generated table of contents
inside the standard marker comments. After editing any of those files, regenerate
the TOC before committing:

```sh
Rscript tools/run_doctoc.R --maxlevel 2 \
  NEWS.md CLAUDE.md CONTRIBUTING.md \
  .github/copilot-instructions.md .github/pull_request_template.md
```

Use `--maxlevel 2` so the TOC only lists `# ` and `## ` headings; level-3
sub-entries crowd the nav. `cran-comments.md` is intentionally excluded.

### README.md (rmarkdown)

`README.md` is rendered from `README.Rmd`. After editing `README.Rmd`, re-render
before committing:

```r
devtools::build_readme()
```

Commit `README.Rmd` and the regenerated `README.md` together. Never hand-edit
`README.md`.

### DESCRIPTION (usethis)

After editing `DESCRIPTION` (adding/removing packages, bumping versions, updating
`Authors@R`, etc.), normalize formatting before committing:

```r
usethis::use_tidy_description()
```

This re-orders fields, alphabetizes `Imports`/`Suggests`, and reflows long lines
so subsequent diffs stay minimal.

### Release notes triad: NEWS.md / cran-comments.md / _pkgdown.yml

Three files describe the same release at different audiences. Whenever you add a
`NEWS.md` bullet, think through all three before committing:

- **`NEWS.md`** -- authoritative changelog for downstream users; rendered into the
  pkgdown changelog. New bullets go under the most recent development version
  heading; do not create a new version section ahead of release.
- **`cran-comments.md`** -- what gets submitted to CRAN. Every behavioral or
  user-visible change in `NEWS.md` should be reflected here before submission.
  Purely internal changes (refactors, test infrastructure, dev tooling) can be
  omitted.
- **`_pkgdown.yml`** -- the pkgdown reference index. New exported functions need to
  land in the right `reference:` section. baseballr uses `starts_with()` selectors
  (e.g. `starts_with("mlb_")`, `starts_with("fg_")`, `starts_with("bref")`) so new
  functions matching those prefixes are picked up automatically; explicitly-listed
  functions need a manual entry. Preview with `pkgdown::build_site()` when in doubt.

When the change touches the API surface (new export, deprecation, removal),
include a one-line note in your commit message confirming you've checked all three.

## Commit Convention

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add mlb_game_context_metrics() endpoint wrapper
fix: initialize return value before tryCatch in fg_team_pitcher()
docs: update NEWS.md for the next dev release
test: add fg_batter_leaders column validation
refactor: migrate ncaa_roster() from %>% to native pipe
chore: update .Rbuildignore patterns
ci: bump actions/checkout to v5
```

Prefer scoped commit subjects when useful (e.g. `feat(mlb): ...`,
`docs(instructions): ...`). Use `type!:` or a `BREAKING CHANGE:` footer for
breaking changes. Split unrelated work into separate commits for reviewability.

**Important**: Never include AI agents or assistants (e.g. Claude, Copilot) as
co-authors on commits. Omit all `Co-Authored-By` trailers referencing AI tools.

## Common Pitfalls

- Always initialize the return variable (`out <- NULL`, etc.) **before** the
  `tryCatch` block -- see Return-Value Initialization above.
- Never pass a raw condition object to a `cli_*` function; pass
  `conditionMessage(cond)` through a value placeholder.
- The NCAA stats site IP-bans scrapers -- run `ncaa_*()` work sparingly and cache
  results; keep those tests skipped on CI.
- Use the native pipe `|>`; do not add new `%>%` usage outside the legacy NCAA
  wrappers that still depend on `magrittr`.
- Drop known-transient columns with `dplyr::select(-dplyr::any_of(...))` so schema
  drift upstream is survivable.
- Upstream sites add columns over time -- use subset-direction assertions in tests.
- `data.frame(list_of_items)` can yield wrong column types when the source returns
  mixed `NULL`/value fields -- supply `%||%` defaults first.
- Never hand-edit `NAMESPACE` or files under `man/`; regenerate with
  `devtools::document()`.
- Local dev artifacts (e.g. `.vscode`, `.claude`) can surface as `R CMD check`
  notes -- keep `.Rbuildignore` current.

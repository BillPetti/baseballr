# Contributing to baseballr

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Contributing to baseballr](#contributing-to-baseballr)
  - [Code of Conduct](#code-of-conduct)
  - [Development Setup](#development-setup)
  - [Workflow](#workflow)
  - [Naming Conventions](#naming-conventions)
  - [Coding Conventions](#coding-conventions)
  - [Testing](#testing)
  - [Documentation Maintenance](#documentation-maintenance)
  - [Commit Messages](#commit-messages)
  - [Pull Requests](#pull-requests)
  - [Reporting Issues](#reporting-issues)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

Thank you for your interest in contributing to **baseballr**! This package
provides clean, tidy baseball data from a range of public sources -- the MLB
Stats API, FanGraphs, Baseball Reference, Baseball Savant (Statcast), the NCAA
baseball stats site, Spotrac, the Chadwick Bureau register, and Retrosheet.
Contributions of all kinds are welcome: bug reports, new endpoint wrappers,
documentation fixes, and tests.

This document covers how to get set up, the conventions the codebase follows,
and what a reviewable pull request looks like. When this guide differs from the
current repository docs, treat `CLAUDE.md` and the current test implementations
as authoritative.

## Code of Conduct

This project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms. Report
unacceptable behavior to the maintainer at <saiem.gilani@gmail.com>.

## Development Setup

1. **Fork** the repository (`BillPetti/baseballr`) to your own account on GitHub.

2. **Clone** your fork locally:

   ```sh
   git clone https://github.com/<your-username>/baseballr.git
   cd baseballr
   ```

3. **Install development dependencies** from within R:

   ```r
   # install.packages("devtools")
   devtools::install_dev_deps()
   ```

4. **Branch from `master`.** `master` is the default and release branch.
   Active development is currently staged on `development_branch`; ask the
   maintainer which base to branch from if you are unsure, but the safe default
   is the latest `master`.

   ```sh
   git checkout master
   git pull
   git checkout -b feat/my-new-endpoint
   ```

5. **Confirm the package builds and checks** before you start:

   ```r
   devtools::load_all()
   devtools::document()
   devtools::check()
   ```

baseballr requires **R (>= 4.1.0)** because the codebase uses the native pipe
(`|>`).

## Workflow

### Build & Development Commands

```r
# Regenerate roxygen documentation + NAMESPACE
devtools::document()

# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-mlb_schedule.R")

# Full R CMD check
devtools::check()

# Install locally
devtools::install()

# Build pkgdown site locally
pkgdown::build_site()
```

### Making Changes

1. Make your changes on a feature branch.
2. Keep code, tests, and roxygen/doc updates in the **same** PR when you change
   exported behavior.
3. Run `devtools::document()` whenever you touch roxygen comments, add a
   function, or change a signature.
4. Run `devtools::test()` and `devtools::check()` before opening the PR.

### Adding a New Endpoint Wrapper

1. Place the function in the appropriate `R/` file for its data source (see the
   prefix table below), or create a new `R/<source>_<topic>.R` file if it does
   not fit an existing one.
2. Follow the function pattern documented in `CLAUDE.md`: initialize the return
   variable **before** the `tryCatch`, parse the payload, run it through the
   `janitor::clean_names()` + `make_baseballr_data()` pipeline, and emit
   `cli` messages from the error/warning handlers.
3. Add a roxygen block with `@title`, `@param`, `@return` (with a column table),
   `@export`, `@family`, and a runnable `@examples` block.
4. Add a test in `tests/testthat/` using the subset-direction column assertions.
5. Run `devtools::document()` to regenerate `man/` and `NAMESPACE`.
6. Add a `NEWS.md` bullet and confirm the function is picked up by the
   `_pkgdown.yml` reference index.

## Naming Conventions

Functions are named by their data source. New wrappers must use the matching
prefix so the pkgdown `starts_with()` selectors pick them up automatically.

| Data Source                      | Prefix         | Example                                  |
| -------------------------------- | -------------- | ---------------------------------------- |
| MLB Stats API                    | `mlb_`         | `mlb_schedule()`, `mlb_pbp()`            |
| FanGraphs                        | `fg_`          | `fg_batter_leaders()`, `fg_team_pitcher()` |
| Baseball Reference               | `bref_`        | `bref_daily_batter()`, `bref_team_results()` |
| Baseball Savant / Statcast       | `statcast_` / `sc_` | `statcast_search()`, `statcast_leaderboards()` |
| ESPN MLB                         | `espn_mlb_`    | `espn_mlb_pbp()`, `espn_mlb_scoreboard()` |
| ESPN College Baseball            | `espn_college_baseball_` | `espn_college_baseball_pbp()`, `espn_college_baseball_scoreboard()` |
| Fox Sports (Bifrost) MLB         | `fox_mlb_`     | `fox_mlb_team_roster()`, `fox_mlb_standings()` |
| NCAA baseball                    | `ncaa_`        | `ncaa_schedule_info()`, `ncaa_roster()`  |
| Spotrac                          | `sptrc_`       | `sptrc_team_active_payroll()`            |
| Chadwick Bureau register         | `chadwick_`    | `chadwick_player_lu()`                   |
| Retrosheet                       | `retrosheet_`  | `retrosheet_data()`                      |
| Metrics                          | `metrics_` (family) | `woba_plus()`, `fip_plus()`         |
| Data loaders                     | `load_`        | `load_ncaa_baseball_pbp()`, `load_umpire_ids()` |
| Visualizations                   | (named)        | `ggspraychart()`                         |

### General Naming Rules

- Use the data-source prefix for every new exported wrapper.
- Use `snake_case` for function and argument names.
- Function default arguments should be a single value (e.g.
  `output = "default"`), with the valid choices documented and validated inside
  the function body -- not a `match.arg`-style `c(...)` choice vector in the
  signature.

### Data Processing Pipeline

Wrappers should funnel their parsed payload through the standard pipeline so the
return object carries the `baseballr_data` class and metadata attributes:

```r
raw_data |>
  janitor::clean_names() |>
  make_baseballr_data("Description of the data from <source>", Sys.time())
```

`make_baseballr_data()` sets the class to
`c("baseballr_data", "tbl_df", "tbl", "data.table", "data.frame")` and attaches
provenance attributes.

### Roxygen Documentation Checklist

- `@title` / `@description`
- `@param` for every argument (including `...`)
- `@return` with a column-name/type table where applicable
- `@export`
- `@family <source> Functions`
- a runnable `@examples` block (wrap live-site calls in `\donttest{}` so
  `R CMD check` does not hit the network during routine checking). If a call
  cannot run under `R CMD check` -- e.g. it needs the optional `chromote` +
  Google Chrome browser fallback (the NCAA `stats.ncaa.org` scrapers) -- show it
  as a code block in the Rd `@details` section instead of `@examples`, so
  `--run-donttest` never launches a browser (a failed headless-Chrome launch
  leaks connections and fails the check).

### Code Style

Follow tidyverse style. The detailed coding conventions -- the native-pipe
rule, return-value initialization, `cli` messaging, and column-drift resilience
-- live in **`CLAUDE.md`**. Read that file before writing wrapper code; the
highlights are summarized below.

## Coding Conventions

The authoritative reference is **`CLAUDE.md`**. Key rules:

- **Native pipe `|>` everywhere.** `magrittr` (`%>%`) is retained as a
  dependency only because some NCAA wrappers still use it; do not introduce new
  `%>%` usage.
- **Return-value initialization (CRITICAL).** Every wrapper that returns a
  variable assigned inside a `tryCatch` must initialize that variable
  (usually `<- NULL`) **before** the `tryCatch`. Otherwise an API error runs the
  error handler, the return variable is never bound, and `return(<var>)` throws
  `object '<var>' not found` instead of returning an empty value with a message.
- **Messaging via `cli`.** Use `cli::cli_alert_danger()` in error handlers,
  `cli::cli_alert_warning()` and `cli::cli_alert_info()` for warnings/info, and
  `cli::cli_warn()` / `cli::cli_abort()` for raised conditions. Never pass a raw
  condition object to a `cli_*` call (the message is glue-interpolated); pass
  `conditionMessage(cond)` through a value placeholder.
- **Column-drift resilience.** Upstream sites add and occasionally rename
  columns. When dropping a known-transient column, use
  `dplyr::select(-dplyr::any_of("colname"))` rather than the bare form so a
  schema change is survivable.

## Testing

Tests live in `tests/testthat/`. Many of them hit live sites and are gated /
skipped so they do not run during routine `R CMD check` or on CI.

### Live-API and NCAA caveats

- **Live-API tests are gated.** Network-dependent tests skip on CRAN and CI and
  should guard against empty/transient responses. Do not assume the network is
  available in a check run.
- **The NCAA stats site aggressively IP-bans scrapers.** Tests and development
  work that hit NCAA endpoints (`ncaa_*`) must be done **sparingly** and cached
  wherever possible. Do not run NCAA tests in a tight loop, and do not add tests
  that hammer NCAA endpoints. A single careless test run can get your IP (or a
  CI runner's IP) banned. When iterating on NCAA wrappers, save a sample payload
  locally and develop against the cached fixture.
- **`stats.ncaa.org` is behind Akamai Bot Manager.** Direct `httr2`/`curl`
  requests get a hard 403 or a soft `bm-verify` interstitial, so
  `request_with_proxy()` falls back to a stealth headless-Chrome fetch
  (`R/ncaa_chromote.R`) via the optional `chromote` + Google Chrome dependency
  (`Suggests`). The live NCAA tests need that browser fallback and are opt-in:
  install `chromote` and set `NCAA_CHROMOTE_TESTS=1` (they `skip_on_cran()` /
  `skip_on_ci()`). Without the dependency the scrapers degrade with a clear
  install message rather than erroring.

### Test Pattern

**Always use the subset direction for column assertions.** Because the upstream
sources add columns over time, a strict `expect_equal` on the full column set
will break the moment a non-breaking column is added. The rule is: the
*expected* column list must be a subset of the *actual* columns.

```r
test_that("mlb_function returns expected columns", {
  skip_on_cran()
  skip_on_ci()

  x <- mlb_function(game_pk = 632970)

  # Skip-if-empty guard -- always right after the API call, before any
  # assertion that touches the data. Handles transient errors and outages.
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from endpoint at test time")
  }

  cols_x <- c("col1", "col2")
  expect_true(all(cols_x %in% colnames(x)))   # expected subset of actual
  expect_s3_class(x, "data.frame")
})
```

**Anti-patterns to avoid:**

```r
# WRONG -- flags when upstream adds a column, even though it is non-breaking
expect_equal(sort(colnames(x)), sort(cols_x))

# WRONG -- same direction problem, just phrased with expect_in
expect_in(sort(colnames(x)), sort(cols_x))
```

For list-returning endpoints that sometimes return fewer elements than expected,
use an inline helper so individual asserts skip gracefully rather than erroring:

```r
check_cols <- function(obj, cols) {
  if (is.null(obj) || !is.data.frame(obj) || ncol(obj) == 0) return(invisible(NULL))
  expect_true(all(cols %in% colnames(obj)))
  expect_s3_class(obj, "data.frame")
}
```

## Documentation Maintenance

Several regeneration steps are part of the commit workflow whenever the relevant
sources change. All of them are mechanical -- never edit the generated regions
by hand.

### Markdown TOCs (doctoc)

`NEWS.md`, `CLAUDE.md`, `CONTRIBUTING.md`, `.github/copilot-instructions.md`, and
`.github/pull_request_template.md` carry a doctoc-generated table of contents
inside the standard marker comments. After editing any of those files,
regenerate the TOC before committing:

```sh
Rscript tools/run_doctoc.R --maxlevel 2 \
  NEWS.md CLAUDE.md CONTRIBUTING.md \
  .github/copilot-instructions.md .github/pull_request_template.md
```

`tools/run_doctoc.R` is a no-deps, idempotent R replacement for the npm `doctoc`
CLI -- it produces output indistinguishable from the upstream tool and runs
without Node.js. Use `--maxlevel 2` so the TOC only lists `# ` and `## `
headings. `cran-comments.md` is intentionally excluded.

### README.md (rmarkdown)

`README.md` is rendered from `README.Rmd`. After editing `README.Rmd`,
re-render before committing:

```r
devtools::build_readme()
```

Commit `README.Rmd` and the regenerated `README.md` together. Never hand-edit
`README.md`.

### DESCRIPTION (usethis)

After editing `DESCRIPTION` (adding/removing packages, bumping versions,
updating `Authors@R`, etc.), normalize formatting before committing:

```r
usethis::use_tidy_description()
```

This re-orders fields, alphabetizes `Imports`/`Suggests`, and reflows long lines
so subsequent diffs stay minimal. Run it even for one-line edits.

### Release notes triad: NEWS.md / cran-comments.md / _pkgdown.yml

Three files describe the same release at different audiences. Whenever you add a
`NEWS.md` bullet, think through all three before committing:

- **`NEWS.md`** -- authoritative changelog for downstream users; rendered into
  the pkgdown changelog. New bullets go under the most recent unreleased version
  heading. Extend an existing subsection (`### Bug fixes`, `### New features`,
  etc.) rather than starting a new one when the change is incremental.
- **`cran-comments.md`** -- the short-form summary submitted to CRAN. Every
  behavioral or user-visible change in `NEWS.md` should be reflected here before
  submission. Purely internal changes (refactors, test infrastructure, dev
  tooling) can be omitted.
- **`_pkgdown.yml`** -- the pkgdown reference index. New exported functions need
  to land in the right `reference:` section. The config uses `starts_with()`
  selectors (`starts_with("mlb_")`, `starts_with("fg_")`, etc.), so new
  functions matching those prefixes are picked up automatically; explicitly
  listed functions need a manual entry.

When a change touches the API surface (new export, deprecation, removal), include
a one-line note in your commit message confirming you've checked all three files.

## Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add mlb_draft_pick_history() endpoint wrapper
fix: initialize return value before tryCatch in fg_team_pitcher()
docs: update NEWS.md for v1.6.0
test: add subset-direction column checks for statcast_search()
refactor: extract NCAA payload parsing into helper
chore: update .Rbuildignore patterns
ci: bump actions/checkout to v5
```

Prefer scoped subjects when useful (e.g. `feat(mlb): ...`, `docs(contrib): ...`).
Use `type!:` or a `BREAKING CHANGE:` footer for breaking changes. Split unrelated
work into separate commits for reviewability.

**Important:** Never include AI tools or assistants (e.g. Claude, Copilot) as
commit co-authors. Omit all `Co-Authored-By` trailers that reference AI tools.

## Pull Requests

- **Target `master`** (the default and release branch).
- Fill out the pull request template completely.
- Ensure `devtools::check()` passes with no new errors, warnings, or notes.
- Ensure `devtools::document()` has been run so `man/` and `NAMESPACE` are
  current.
- Add or update tests for any changed behavior, respecting the live-API and
  NCAA IP-ban caveats above.
- Add a `NEWS.md` bullet (and reflect user-visible changes in `cran-comments.md`
  and `_pkgdown.yml` where applicable).
- Keep the PR focused; split unrelated changes into separate PRs.

## Reporting Issues

When filing a bug report, please include:

- A minimal **reprex** (reproducible example) showing the call you made.
- The relevant identifier(s) you passed (e.g. `game_pk`, FanGraphs player id,
  season, team).
- The output of `sessionInfo()`.
- The full error or warning message.

For NCAA-related issues, please note whether you may have been rate-limited or
IP-banned (the symptom is usually an HTTP error or an empty/timeout response
after repeated requests).

## License

baseballr is released under the [MIT License](LICENSE.md). By contributing, you
agree that your contributions will be licensed under the same terms.

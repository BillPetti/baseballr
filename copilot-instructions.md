# GitHub Copilot Instructions – baseballr

These instructions tell GitHub Copilot (and other AI coding assistants)
how to write code that fits the `baseballr` package. They mirror the
fuller [`CLAUDE.md`](https://billpetti.github.io/CLAUDE.md) development
guide – when the two differ, treat `CLAUDE.md`, `CONTRIBUTING.md`, and
the current tests as authoritative.

## Package summary

`baseballr` acquires and analyzes baseball data from several online
sources. It is an R package (\>= 4.1.0, MIT licensed). The default and
release branch is `master`; active development happens on feature
branches and `development_branch`.

## Function naming by data source

Use the prefix that matches the data source. Never mix sources behind
one prefix.

| Prefix | Source | Example |
|----|----|----|
| `mlb_` | MLB Stats API (`statsapi.mlb.com`) | [`mlb_pbp()`](https://billpetti.github.io/baseballr/reference/mlb_pbp.md), [`mlb_teams()`](https://billpetti.github.io/baseballr/reference/mlb_teams.md) |
| `fg_` | FanGraphs | [`fg_batter_leaders()`](https://billpetti.github.io/baseballr/reference/fg_batter_leaders.md) |
| `bref_` | Baseball Reference | [`bref_team_results()`](https://billpetti.github.io/baseballr/reference/bref_team_results.md) |
| `statcast_` / `sc_` | Baseball Savant / Statcast | [`statcast_search()`](https://billpetti.github.io/baseballr/reference/statcast_search.md) |
| `espn_mlb_` | ESPN (MLB) | [`espn_mlb_pbp()`](https://billpetti.github.io/baseballr/reference/espn_mlb_pbp.md), [`espn_mlb_scoreboard()`](https://billpetti.github.io/baseballr/reference/espn_mlb_scoreboard.md) |
| `ncaa_` | NCAA baseball stats site | [`ncaa_team_player_stats()`](https://billpetti.github.io/baseballr/reference/ncaa_team_player_stats.md) |
| `sptrc_` | Spotrac | [`sptrc_team_active_payroll()`](https://billpetti.github.io/baseballr/reference/sptrc_team_active_payroll.md) |
| `chadwick_` | Chadwick Bureau register | [`chadwick_player_lu()`](https://billpetti.github.io/baseballr/reference/chadwick_player_lu.md) |
| `retrosheet_` | Retrosheet | [`retrosheet_data()`](https://billpetti.github.io/baseballr/reference/retrosheet_data.md) |
| `metrics_` | Derived metrics | `metrics_woba_plus()` |
| `load_` | Cached data-repository loaders | [`load_umpire_ids()`](https://billpetti.github.io/baseballr/reference/load_umpire_ids.md) |

## Mandatory conventions

- **Native pipe.** Use `|>`, not `%>%`. `magrittr` is retained as a
  dependency **only** because the NCAA wrappers still use `%>%`; do not
  add new `%>%` usage.

- **Return-value initialization (critical).** Any wrapper that returns a
  variable assigned inside
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) MUST initialize
  that variable (usually `<- NULL`) **before** the `tryCatch` block.
  Otherwise an API error leaves the variable unbound and
  [`return()`](https://rdrr.io/r/base/function.html) throws
  `object '<var>' not found` instead of failing gracefully. Also guard
  any post-`tryCatch` processing (e.g.
  `if (!is.null(x)) colnames(x) <- ...`).

  ``` r

  my_fn <- function(...) {
    df <- NULL                      # initialize BEFORE tryCatch
    tryCatch(
      expr = { df <- ... |> make_baseballr_data("desc", Sys.time()) },
      error = function(e) cli::cli_alert_danger("{conditionMessage(e)}")
    )
    return(df)
  }
  ```

- **Messaging via `cli`.** Use
  [`cli::cli_alert_danger()`](https://cli.r-lib.org/reference/cli_alert.html)
  in error handlers,
  [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html)
  /
  [`cli::cli_alert_info()`](https://cli.r-lib.org/reference/cli_alert.html)
  elsewhere, and
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html) /
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  for raised conditions. Never pass a raw condition object to
  `cli::cli_*()` – the text is glue-interpolated, so a message
  containing `{` will error. Pass `conditionMessage(cond)` via a value
  placeholder: `cli::cli_alert_danger("{conditionMessage(cond)}")`.

- **Data pipeline.** Finish transforms with
  `... |> janitor::clean_names() |> make_baseballr_data("<description>", Sys.time())`.
  `make_baseballr_data()` sets class
  `c("baseballr_data", "tbl_df", "tbl", "data.table", "data.frame")` and
  attaches `baseballr_timestamp` / `baseballr_type` attributes.

- **Column-drift resilience.** Drop columns with
  `dplyr::select(-dplyr::any_of("col"))`, never bare `select(-"col")`,
  because upstream sources add and remove columns over time.

- **Null-safe defaults.** Use `%||%` for fallbacks.

- **Default arguments.** Give a single default value
  (e.g. `output = "default"`), document the choices, and validate inside
  the function. Do not use `match.arg`-style `c(...)` choice-vector
  defaults in signatures.

- **HTTP helpers / FanGraphs.** Don’t call `httr`/`httr2` directly; use
  the source helpers (`mlb_api_call()`, `fg_api_call()`,
  [`request_with_proxy()`](https://billpetti.github.io/baseballr/reference/request_with_proxy.md),
  [`.retry_request()`](https://billpetti.github.io/baseballr/reference/dot-retry_request.md)).
  FanGraphs sits behind Cloudflare and 403s plain/library `User-Agent`s,
  so route FanGraphs requests through `fg_api_call()` – it sends the
  Cloudflare-exempt `okhttp/4.12.0` UA. `mlb_api_call()` keeps a plain
  UA for the MLB Stats API. (okhttp-UA approach adapted from upstream PR
  \#405.)

- **ESPN MLB (`espn_mlb_*`).** Public wrappers in `R/espn_mlb_*.R` are
  thin shims over `R/espn_baseball_*_helpers.R`; the game-summary box
  parsers live in `R/espn_mlb_box_helpers.R`. They share the `httr2`
  layer in `R/utils_espn.R`
  ([`.retry_request()`](https://billpetti.github.io/baseballr/reference/dot-retry_request.md),
  honouring `options(baseballr.proxy = ...)`) and report via
  [`.report_api_error()`](https://billpetti.github.io/baseballr/reference/dot-report_api_error.md)
  /
  [`.report_api_warning()`](https://billpetti.github.io/baseballr/reference/dot-report_api_warning.md),
  not raw `cli::cli_alert_*()`.

## Testing

- Tests live in `tests/testthat/`. Many hit live endpoints and are
  gated/skipped (`skip_on_cran()`, `skip_on_ci()`, source-specific
  guards).
- Assert columns with the **subset direction**: the expected names must
  be a subset of the actual names
  (`expect_in(sort(expected), sort(colnames(x)))`), so a new upstream
  column does not break the test.
- **NCAA caution:** the NCAA stats site aggressively IP-bans scrapers.
  Do not add tests or run dev code that hammers NCAA endpoints; cache
  fixtures instead.

## Documentation & generated files

- `man/` and `NAMESPACE` are roxygen2-generated – never hand-edit them;
  run `devtools::document()`.
- New exported functions must be picked up by `_pkgdown.yml` (family
  sections use `starts_with()` selectors).
- After changing exported behavior, update `NEWS.md` and keep
  `cran-comments.md` in sync for user-visible changes.

## Commits

- Use [Conventional Commits](https://www.conventionalcommits.org/)
  (`feat:`, `fix:`, `docs:`, `test:`, `refactor:`, `chore:`).
- **Never** add AI tools as commit co-authors. Omit any `Co-Authored-By`
  trailer that references an AI assistant.

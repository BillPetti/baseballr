# GitHub Copilot Instructions -- baseballr

These instructions tell GitHub Copilot (and other AI coding assistants) how to
write code that fits the `baseballr` package. They mirror the fuller
[`CLAUDE.md`](../CLAUDE.md) development guide -- when the two differ, treat
`CLAUDE.md`, `CONTRIBUTING.md`, and the current tests as authoritative.

## Package summary

`baseballr` acquires and analyzes baseball data from several online sources. It
is an R package (>= 4.1.0, MIT licensed). The default and release branch is
`master`; active development happens on feature branches and `development_branch`.

## Function naming by data source

Use the prefix that matches the data source. Never mix sources behind one prefix.

| Prefix         | Source                                   | Example                       |
| -------------- | ---------------------------------------- | ----------------------------- |
| `mlb_`         | MLB Stats API (`statsapi.mlb.com`)       | `mlb_pbp()`, `mlb_teams()`    |
| `fg_`          | FanGraphs                                | `fg_batter_leaders()`         |
| `bref_`        | Baseball Reference                       | `bref_team_results()`         |
| `statcast_` / `sc_` | Baseball Savant / Statcast          | `statcast_search()`           |
| `ncaa_`        | NCAA baseball stats site                 | `ncaa_team_player_stats()`    |
| `sptrc_`       | Spotrac                                  | `sptrc_team_active_payroll()` |
| `chadwick_`    | Chadwick Bureau register                 | `chadwick_player_lu()`        |
| `retrosheet_`  | Retrosheet                               | `retrosheet_data()`           |
| `metrics_`     | Derived metrics                          | `metrics_woba_plus()`         |
| `load_`        | Cached data-repository loaders           | `load_umpire_ids()`           |

## Mandatory conventions

- **Native pipe.** Use `|>`, not `%>%`. `magrittr` is retained as a dependency
  **only** because the NCAA wrappers still use `%>%`; do not add new `%>%` usage.
- **Return-value initialization (critical).** Any wrapper that returns a variable
  assigned inside `tryCatch()` MUST initialize that variable (usually `<- NULL`)
  **before** the `tryCatch` block. Otherwise an API error leaves the variable
  unbound and `return()` throws `object '<var>' not found` instead of failing
  gracefully. Also guard any post-`tryCatch` processing (e.g.
  `if (!is.null(x)) colnames(x) <- ...`).

  ```r
  my_fn <- function(...) {
    df <- NULL                      # initialize BEFORE tryCatch
    tryCatch(
      expr = { df <- ... |> make_baseballr_data("desc", Sys.time()) },
      error = function(e) cli::cli_alert_danger("{conditionMessage(e)}")
    )
    return(df)
  }
  ```

- **Messaging via `cli`.** Use `cli::cli_alert_danger()` in error handlers,
  `cli::cli_alert_warning()` / `cli::cli_alert_info()` elsewhere, and
  `cli::cli_warn()` / `cli::cli_abort()` for raised conditions. Never pass a raw
  condition object to `cli::cli_*()` -- the text is glue-interpolated, so a
  message containing `{` will error. Pass `conditionMessage(cond)` via a value
  placeholder: `cli::cli_alert_danger("{conditionMessage(cond)}")`.
- **Data pipeline.** Finish transforms with
  `... |> janitor::clean_names() |> make_baseballr_data("<description>", Sys.time())`.
  `make_baseballr_data()` sets class
  `c("baseballr_data", "tbl_df", "tbl", "data.table", "data.frame")` and attaches
  `baseballr_timestamp` / `baseballr_type` attributes.
- **Column-drift resilience.** Drop columns with
  `dplyr::select(-dplyr::any_of("col"))`, never bare `select(-"col")`, because
  upstream sources add and remove columns over time.
- **Null-safe defaults.** Use `%||%` for fallbacks.
- **Default arguments.** Give a single default value (e.g. `output = "default"`),
  document the choices, and validate inside the function. Do not use
  `match.arg`-style `c(...)` choice-vector defaults in signatures.
- **HTTP helpers / FanGraphs.** Don't call `httr`/`httr2` directly; use the
  source helpers (`mlb_api_call()`, `fg_api_call()`, `request_with_proxy()`).
  FanGraphs sits behind Cloudflare and 403s plain/library `User-Agent`s, so route
  FanGraphs requests through `fg_api_call()` -- it sends the Cloudflare-exempt
  `okhttp/4.12.0` UA. `mlb_api_call()` keeps a plain UA for the MLB Stats API.
  (okhttp-UA approach adapted from upstream PR #405.)

## Testing

- Tests live in `tests/testthat/`. Many hit live endpoints and are gated/skipped
  (`skip_on_cran()`, `skip_on_ci()`, source-specific guards).
- Assert columns with the **subset direction**: the expected names must be a
  subset of the actual names (`expect_in(sort(expected), sort(colnames(x)))`),
  so a new upstream column does not break the test.
- **NCAA caution:** the NCAA stats site aggressively IP-bans scrapers. Do not add
  tests or run dev code that hammers NCAA endpoints; cache fixtures instead.

## Documentation & generated files

- `man/` and `NAMESPACE` are roxygen2-generated -- never hand-edit them; run
  `devtools::document()`.
- New exported functions must be picked up by `_pkgdown.yml` (family sections use
  `starts_with()` selectors).
- After changing exported behavior, update `NEWS.md` and keep `cran-comments.md`
  in sync for user-visible changes.

## Commits

- Use [Conventional Commits](https://www.conventionalcommits.org/)
  (`feat:`, `fix:`, `docs:`, `test:`, `refactor:`, `chore:`).
- **Never** add AI tools as commit co-authors. Omit any `Co-Authored-By` trailer
  that references an AI assistant.

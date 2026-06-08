# Pull Request

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Pull Request](#pull-request)
  - [Summary](#summary)
  - [Type of Change](#type-of-change)
  - [Related Issues](#related-issues)
  - [Background & Context](#background--context)
  - [Changes Made](#changes-made)
  - [Submission Checklist](#submission-checklist)
  - [Testing](#testing)
  - [Screenshots / Output](#screenshots--output)
  - [Reviewer Checklist](#reviewer-checklist)
  - [Additional Notes](#additional-notes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Summary

<!-- A clear, concise description of what this PR does and why. -->

## Type of Change

<!-- Check all that apply. -->

- [ ] `feat` -- New feature (new endpoint wrapper, new exported function)
- [ ] `fix` -- Bug fix (non-breaking change that fixes an issue)
- [ ] `docs` -- Documentation only (roxygen, README, vignettes, NEWS)
- [ ] `test` -- Adding or updating tests
- [ ] `refactor` -- Code change that neither fixes a bug nor adds a feature
- [ ] `chore` -- Tooling, build, or maintenance change
- [ ] `perf` -- Performance improvement

<!-- If this is a breaking change, note it here and use `type!:` /
     `BREAKING CHANGE:` in the commit. -->

## Related Issues

<!-- Link issues this PR closes or relates to, e.g. "Closes #123". -->

## Background & Context

<!-- Why is this change needed? Link any relevant Discord threads or
     discussions. For data-source changes, note which source (MLB Stats API,
     FanGraphs, Baseball Reference, Baseball Savant, NCAA, Spotrac, Chadwick,
     Retrosheet) is affected. -->

## Changes Made

<!-- Summarize the concrete changes. Fill in the table below. -->

| File / Function | Change |
| --------------- | ------ |
|                 |        |
|                 |        |
|                 |        |

## Submission Checklist

- [ ] Code follows tidyverse style and the conventions in `CLAUDE.md`
      (native pipe `|>`, return-value initialized before `tryCatch`, `cli`
      messaging, `any_of()` column-drift guards).
- [ ] I ran `devtools::document()` and committed the regenerated `man/` and
      `NAMESPACE` (no hand edits).
- [ ] Roxygen blocks are complete (`@param`, `@return`, `@export`, `@family`,
      runnable `@examples`).
- [ ] I added or updated tests, respecting the live-API gating and the NCAA
      IP-ban caveat (NCAA endpoints exercised sparingly / against cached data).
- [ ] `devtools::check()` passes with no new errors, warnings, or notes.
- [ ] I updated `NEWS.md`, and reflected user-visible changes in
      `cran-comments.md` and `_pkgdown.yml` where applicable.
- [ ] Commits follow Conventional Commits and contain **no** AI co-author
      trailers.

## Testing

<!-- Describe how you tested these changes. -->

- [ ] `devtools::test()` passes locally.
- [ ] New/changed behavior is covered by a test using subset-direction column
      assertions.

<!-- For changes that affect runtime, note execution time before and after if
     relevant. -->

Execution time before: <!-- e.g. n/a -->
Execution time after:  <!-- e.g. n/a -->

## Screenshots / Output

<!-- Paste relevant console output, sample tibbles, or plots (e.g. for
     ggspraychart changes). -->

## Reviewer Checklist

<!-- For the reviewer. Do not approve until all items are verified. -->

- [ ] Function naming uses the correct data-source prefix.
- [ ] Return value is initialized before `tryCatch`; error path returns an
      empty value with a `cli` message rather than erroring.
- [ ] `man/` and `NAMESPACE` are regenerated and consistent with the source.
- [ ] Tests are appropriately gated; no test hammers the NCAA stats site.
- [ ] `NEWS.md` / `cran-comments.md` / `_pkgdown.yml` are consistent for any
      API-surface change.
- [ ] Commit messages are conventional and free of AI co-author trailers.

## Additional Notes

<!-- Anything else the reviewer should know. -->

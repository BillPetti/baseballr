---
name: Bug report
about: Report a problem with a baseballr function so we can fix it
title: "[Bug]: "
labels: ["bug"]
assignees: ''
---

**Describe the bug**

A clear and concise description of what the bug is, including the full error
message or unexpected output you received.

**Which data source / function?**

Which family of functions is affected, and which specific function did you call?

- [ ] MLB Stats API (`mlb_*`)
- [ ] Baseball Savant / Statcast (`statcast_*`, `scrape_statcast_savant*`)
- [ ] FanGraphs (`fg_*`)
- [ ] Baseball Reference (`bref_*`)
- [ ] NCAA (`ncaa_*`, `load_ncaa_baseball_*`)
- [ ] Retrosheet (`retrosheet_data`)
- [ ] Chadwick Bureau lookup (`chadwick_*`, `playerid_lookup`, `playername_lookup`)
- [ ] Spotrac (`sptrc_*`)
- [ ] Metrics / visualization (`woba_plus`, `fip_plus`, `ggspraychart`, ...)
- [ ] Other / not sure

Function called: `e.g. mlb_pbp(game_pk = 632970)`

**Reproducible example**

Please provide a minimal, self-contained example that reproduces the problem.
A [reprex](https://reprex.tidyverse.org/) is ideal. Include the arguments you
passed (game IDs, season, player IDs, dates, etc.) so we can reproduce exactly
what you ran.

```r
library(baseballr)

# Your reproducible example here
```

**Expected behavior**

A clear and concise description of what you expected to happen.

**Actual behavior**

What actually happened (paste the error message, warning, or unexpected
output here).

**Version information**

Please report your baseballr and R versions:

```r
packageVersion("baseballr")
R.version.string
# or, for full detail:
# sessionInfo()
```

- baseballr version:
- R version:
- OS:

**Additional context**

Add any other context about the problem here (e.g. is the upstream data source
reachable in a browser? Is this intermittent?).

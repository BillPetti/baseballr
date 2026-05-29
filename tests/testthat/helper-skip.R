# Source-specific skip helpers for baseballr's live-API tests.
#
# Every test that hits an external service is gated behind one of these helpers
# so that `R CMD check` (e.g. on CI) does not fail when an upstream site is
# down, rate-limiting, has changed its schema, or is blocking automated access.
# A test only runs when its corresponding environment variable is set to "1":
#
#   FANGRAPHS_TESTS=1   FanGraphs           (fg_*)
#   MLB_STATS_TESTS=1   MLB Stats API       (mlb_*)
#   BREF_TESTS=1        Baseball Reference  (bref_*)
#   STATCAST_TESTS=1    Baseball Savant     (statcast_*, label_statcast_*)
#   NCAA_TESTS=1        NCAA stats site     (ncaa_*)  -- see note below
#   SPOTRAC_TESTS=1     Spotrac             (sptrc_*)
#   CHADWICK_TESTS=1    Chadwick register   (chadwick_*)
#   BASEBALLR_LOAD_TESTS=1  data-repo loaders (load_*)
#
# Example, to run the MLB Stats API tests locally:
#   Sys.setenv(MLB_STATS_TESTS = "1"); devtools::test()
#
# NOTE: the NCAA stats site aggressively IP-bans scrapers. Only set
# NCAA_TESTS=1 deliberately and sparingly, ideally from an IP you are willing
# to have rate-limited.

skip_fangraphs_test <- function() {
  if (Sys.getenv("FANGRAPHS_TESTS") != "1") {
    skip("Set FANGRAPHS_TESTS=1 to run FanGraphs tests")
  } else {
    invisible()
  }
}

skip_mlb_test <- function() {
  if (Sys.getenv("MLB_STATS_TESTS") != "1") {
    skip("Set MLB_STATS_TESTS=1 to run MLB Stats API tests")
  } else {
    invisible()
  }
}

skip_bref_test <- function() {
  if (Sys.getenv("BREF_TESTS") != "1") {
    skip("Set BREF_TESTS=1 to run Baseball Reference tests")
  } else {
    invisible()
  }
}

skip_statcast_test <- function() {
  if (Sys.getenv("STATCAST_TESTS") != "1") {
    skip("Set STATCAST_TESTS=1 to run Baseball Savant / Statcast tests")
  } else {
    invisible()
  }
}

skip_ncaa_test <- function() {
  if (Sys.getenv("NCAA_TESTS") != "1") {
    skip("Set NCAA_TESTS=1 to run NCAA tests (note: NCAA IP-bans scrapers)")
  } else {
    invisible()
  }
}

skip_sptrc_test <- function() {
  if (Sys.getenv("SPOTRAC_TESTS") != "1") {
    skip("Set SPOTRAC_TESTS=1 to run Spotrac tests")
  } else {
    invisible()
  }
}

skip_chadwick_test <- function() {
  if (Sys.getenv("CHADWICK_TESTS") != "1") {
    skip("Set CHADWICK_TESTS=1 to run Chadwick register tests")
  } else {
    invisible()
  }
}

skip_load_test <- function() {
  if (Sys.getenv("BASEBALLR_LOAD_TESTS") != "1") {
    skip("Set BASEBALLR_LOAD_TESTS=1 to run load_* data-repository tests")
  } else {
    invisible()
  }
}

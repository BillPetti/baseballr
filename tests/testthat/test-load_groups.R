# Offline tests stub csv_from_url() with rows copied verbatim from the published
# mlb_groups / ncaa_baseball_groups release assets; the last test is the gated
# live smoke call against the release itself.

groups_fixture <- function(text) {
  seen <- character()
  local_mocked_bindings(
    csv_from_url = function(input, ...) {
      seen <<- c(seen, input)
      data.table::fread(text = text, ...)
    },
    .env = parent.frame()
  )
  function() seen
}

test_that("load_mlb_team_group_seasons builds per-season urls and keeps contract dtypes", {
  skip_on_cran()
  seen <- groups_fixture(paste(
    "league,season,team_id,team_id_source,team_name,subdivision_id,conference_id,division_id,source,sources_agree,notes",
    "mlb,2013,18,espn,Houston Astros,,mlb:al,mlb:al-west,mlb,true,mlb_team_id=117",
    sep = "\n"
  ))
  x <- load_mlb_team_group_seasons(seasons = c(2012, 2013))

  expect_equal(
    basename(seen()),
    c("mlb_team_group_seasons_2012.csv", "mlb_team_group_seasons_2013.csv")
  )
  expect_match(seen()[1], "/releases/download/mlb_groups/", fixed = TRUE)
  expect_s3_class(x, "baseballr_data")
  expect_type(x$team_id, "character")
  expect_equal(x$team_id[1], "18")
  expect_type(x$season, "integer")
  expect_type(x$subdivision_id, "character")
  expect_true(is.na(x$subdivision_id[1]))
  expect_type(x$sources_agree, "logical")
  expect_equal(x$division_id[1], "mlb:al-west")
})

test_that("load_ncaa_baseball_group_aliases reads the season-less file with character ids", {
  skip_on_cran()
  seen <- groups_fixture(paste(
    "league,group_id,source,source_id,name_kind,value,valid_from,valid_to",
    "ncaa_baseball,ncaa_baseball:acc,ncaa,821,short_name,ACC,2010,2026",
    "ncaa_baseball,ncaa_baseball:acc,ncaa,821,slug,acc,,",
    sep = "\n"
  ))
  x <- load_ncaa_baseball_group_aliases()

  expect_equal(basename(seen()), "ncaa_baseball_group_aliases.csv")
  expect_match(seen(), "/releases/download/ncaa_baseball_groups/", fixed = TRUE)
  expect_type(x$source_id, "character")
  expect_equal(x$source_id[1], "821")
  expect_type(x$valid_to, "integer")
  expect_true(is.na(x$valid_to[2]))
})

test_that("seasons = TRUE reads the all-seasons file", {
  skip_on_cran()
  seen <- groups_fixture(paste(
    "league,season,team_id,team_id_source,team_name,subdivision_id,conference_id,division_id,source,sources_agree,notes",
    "mlb,2013,18,espn,Houston Astros,,mlb:al,mlb:al-west,mlb,true,mlb_team_id=117",
    sep = "\n"
  ))
  load_mlb_team_group_seasons(seasons = TRUE)
  expect_equal(basename(seen()), "mlb_team_group_seasons.csv")
})

test_that("group loaders reject seasons before the league's first season", {
  skip_on_cran()
  expect_error(load_mlb_team_group_seasons(seasons = 1900))
  expect_error(load_ncaa_baseball_team_group_seasons(seasons = 2009))
  expect_error(load_mlb_team_group_seasons(seasons = 2012.5))
})

test_that("a failed download warns and returns the contract columns", {
  skip_on_cran()
  local_mocked_bindings(csv_from_url = function(...) stop("HTTP status was '404 Not Found'"))
  expect_warning(x <- load_mlb_groups(), "404")
  expect_equal(nrow(x), 0)
  expect_equal(
    colnames(x),
    c("league", "group_id", "level", "first_season", "last_season", "notes")
  )
})

test_that("load_mlb_team_group_seasons live: Astros move to the AL West in 2013", {
  skip_on_cran()
  skip_load_test()
  x <- load_mlb_team_group_seasons(seasons = 2012:2013)
  expect_setequal(unique(x$season), c(2012L, 2013L))
  cols <- c("league", "season", "team_id", "team_name", "conference_id", "division_id")
  expect_in(sort(cols), sort(colnames(x)))
  astros <- x[x$team_id == "18", ]
  expect_equal(astros$division_id[order(astros$season)], c("mlb:nl-central", "mlb:al-west"))
})

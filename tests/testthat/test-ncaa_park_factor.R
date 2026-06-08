# These tests are fully mocked and never touch stats.ncaa.org. The NCAA stats
# site aggressively IP-bans scrapers, so ncaa_schedule_info() and
# load_ncaa_baseball_teams() are stubbed with local_mocked_bindings() to
# exercise ncaa_park_factor()'s own logic without any network access.

teams_fixture <- data.frame(
  team_id       = c(736, 100, 101, 102, 103, 104),
  conference_id = rep(5L, 6),
  year          = rep(2023L, 6),
  division      = rep(1L, 6),
  conference    = rep("Test Conference", 6),
  team_name     = c("Test School", "A", "B", "C", "D", "E"),
  stringsAsFactors = FALSE
)

schedule_fixture <- data.frame(
  home_team_id    = c(736, 736, 100, 101),
  neutral_site    = c(NA, NA, NA, NA),
  home_team_score = c(5, 7, 3, 4),
  away_team_score = c(3, 2, 6, 8),
  stringsAsFactors = FALSE
)

# Regression for #302: when the schedule comes back empty (e.g. the NCAA site is
# blocking requests), the function used to error mid-pipeline with
# "Column `home_team_id` not found". It must now warn and return an empty frame.
test_that("ncaa_park_factor fails gracefully when the NCAA schedule is unavailable (#302)", {
  testthat::local_mocked_bindings(
    load_ncaa_baseball_teams = function(...) teams_fixture,
    ncaa_schedule_info       = function(...) data.frame()
  )

  expect_warning(
    res <- ncaa_park_factor(team_id = 736, years = 2023, type = "conference"),
    "No NCAA schedule data"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_true(all(c("school", "home_game", "final_pf") %in% names(res)))
})

# Guard must not change the happy path: a valid schedule still yields a park factor.
test_that("ncaa_park_factor computes a park factor from a valid schedule (#302)", {
  testthat::local_mocked_bindings(
    load_ncaa_baseball_teams = function(...) teams_fixture,
    ncaa_schedule_info       = function(...) schedule_fixture
  )

  res <- ncaa_park_factor(team_id = 736, years = 2023, type = "conference")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_true("final_pf" %in% names(res))
  expect_true(is.numeric(res$final_pf))
  expect_false(is.na(res$final_pf))
})

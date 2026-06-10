# Live tests for the Fox Sports (Bifrost) MLB wrappers (generic; Fox exposes no
# MLB pbp/boxscore). Stable id captured 2026-06-10: team 1. Subset-direction
# column checks; skip-if-empty guards for transient API errors.

test_that("Fox MLB team roster", {
  skip_on_cran()
  x <- fox_mlb_team_roster("1")
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No Fox MLB roster at test time")
  expect_in(c("team_id", "position_group", "player", "athlete_id"), colnames(x))
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("Fox MLB team stats + gamelog", {
  skip_on_cran()
  st <- fox_mlb_team_stats("1")
  if (is.null(st) || !is.data.frame(st) || nrow(st) == 0) skip("No Fox MLB team stats at test time")
  expect_in(c("team_id", "category", "stat", "player", "value"), colnames(st))
  Sys.sleep(1)
  gl <- fox_mlb_team_gamelog("1")
  if (!is.null(gl) && nrow(gl) > 0) expect_in(c("team_id", "category", "game_id", "stat", "value"), colnames(gl))
  Sys.sleep(1)
})

test_that("Fox MLB standings + league leaders", {
  skip_on_cran()
  sd <- fox_mlb_standings("1")
  if (is.null(sd) || !is.data.frame(sd) || nrow(sd) == 0) skip("No Fox MLB standings at test time")
  expect_in(c("team_id", "section", "entity_id"), colnames(sd))
  Sys.sleep(1)
  ll <- fox_mlb_league_leaders("batting")
  if (!is.null(ll) && nrow(ll) > 0) expect_in("entity_id", colnames(ll))
  Sys.sleep(1)
})

test_that("Fox MLB odds (ephemeral market tolerated)", {
  skip_on_cran()
  x <- fox_mlb_odds("95687")
  expect_s3_class(x, "data.frame")
  if (!is.null(x) && nrow(x) > 0) expect_in(c("game_id", "team"), colnames(x))
  Sys.sleep(1)
})

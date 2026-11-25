# statcast_search() with Baseball Savant's CSV export mocked: csv_from_url()
# reads a CSV written from rows of fixtures/statcast_one_game.rds (a real
# 2024 game) instead of the network.

.mock_savant_csv <- function(rows) {
  path <- tempfile(fileext = ".csv")
  data.table::fwrite(rows, path)
  function(url, ...) data.table::fread(path, ...)
}

test_that("statcast_search() returns a genuine one-row result", {
  skip_on_cran()
  # A single-pitch query returns one row; until 2.1.0 it was dropped with
  # "No valid data found" because the check was nrow(payload) > 1.
  game <- readRDS(test_path("fixtures", "statcast_one_game.rds"))
  local_mocked_bindings(csv_from_url = .mock_savant_csv(game[1, , drop = FALSE]))
  expect_no_warning(
    out <- suppressMessages(statcast_search("2024-04-01", "2024-04-01", playerid = game$pitcher[[1]],
                                            player_type = "pitcher"))
  )
  expect_s3_class(out, "baseballr_data")
  expect_equal(nrow(out), 1L)
  expect_identical(out$game_pk, as.numeric(game$game_pk[[1]]))
  expect_s3_class(out$game_date, "Date")
})

test_that("statcast_search() still warns on an empty export", {
  skip_on_cran()
  game <- readRDS(test_path("fixtures", "statcast_one_game.rds"))
  # Header row only, as Savant answers a search with no pitches.
  local_mocked_bindings(csv_from_url = .mock_savant_csv(game[0, , drop = FALSE]))
  expect_warning(out <- suppressMessages(statcast_search("2024-04-01", "2024-04-01")),
                 "No valid data found")
  expect_equal(nrow(out), 0L)
  # An empty body.
  local_mocked_bindings(csv_from_url = function(...) data.table::data.table())
  expect_warning(out <- suppressMessages(statcast_search("2024-04-01", "2024-04-01")),
                 "No valid data found")
  expect_equal(nrow(out), 0L)
})

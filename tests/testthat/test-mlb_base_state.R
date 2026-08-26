# Per-event base-state reconstruction (#276), validated against Statcast's
# independently-published per-pitch on_1b/on_2b/on_3b columns.

test_that("mlb_pbp add_base_state matches Statcast per-pitch base state", {
  skip_on_cran()
  skip_if_offline("statsapi.mlb.com")

  pbp <- mlb_pbp(game_pk = 745980, add_base_state = TRUE)  # extra-innings game
  expect_in(c("pre_on_1b", "pre_on_2b", "pre_on_3b"), colnames(pbp))

  mine <- as.data.frame(pbp[pbp$isPitch == TRUE,
                        c("atBatIndex", "pitchNumber", "pre_on_1b", "pre_on_2b", "pre_on_3b")])
  mine$atBatIndex <- as.integer(mine$atBatIndex) + 1  # statcast is 1-based
  mine$pitchNumber <- as.integer(mine$pitchNumber)

  sc <- suppressMessages(try(statcast_search("2024-06-01", "2024-06-01"), silent = TRUE))
  skip_if(inherits(sc, "try-error") || is.null(sc) || nrow(sc) == 0,
          "statcast unavailable")
  sc <- as.data.frame(sc[sc$game_pk == 745980,
                     c("at_bat_number", "pitch_number", "on_1b", "on_2b", "on_3b")])

  m <- merge(mine, sc, by.x = c("atBatIndex", "pitchNumber"),
             by.y = c("at_bat_number", "pitch_number"))
  expect_gt(nrow(m), 250)
  same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  ok <- same(m$pre_on_1b, m$on_1b) & same(m$pre_on_2b, m$on_2b) & same(m$pre_on_3b, m$on_3b)
  expect_equal(mean(ok), 1)
})

test_that("mlb_pbp default output carries no base-state columns", {
  skip_on_cran()
  skip_if_offline("statsapi.mlb.com")
  pbp <- mlb_pbp(game_pk = 745980)
  expect_false(any(c("pre_on_1b", "pre_on_2b", "pre_on_3b") %in% colnames(pbp)))
})

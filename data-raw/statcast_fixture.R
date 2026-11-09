# Build tests/testthat/fixtures/statcast_one_game.rds: every pitch of one 2024 game,
# used by the offline tests of the Statcast-based metric helpers
# (run_expectancy_code(), linear_weights_savant(), statline_from_statcast(),
# edge_code(), ggspraychart()).
# Run from the package root (needs network):
#   Rscript data-raw/statcast_fixture.R

suppressMessages(devtools::load_all(".", quiet = TRUE))

day <- statcast_search(start_date = "2024-04-01", end_date = "2024-04-01")
game <- sort(unique(day$game_pk))[[1]]
one <- day[day$game_pk == game, , drop = FALSE]
one <- as.data.frame(one)
dir.create(file.path("tests", "testthat", "fixtures"), showWarnings = FALSE)
saveRDS(one, file.path("tests", "testthat", "fixtures", "statcast_one_game.rds"),
        compress = "xz", version = 2)
message("game_pk ", game, ": ", nrow(one), " pitches, ", ncol(one), " columns")

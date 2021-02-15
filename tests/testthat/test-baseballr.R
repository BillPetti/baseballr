context("baseballr")

test_that("scraper works", {
  correa <- scrape_statcast_savant(start_date = "2016-04-15",
                                   end_date = "2016-04-15",
                                   playerid = 621043)
  expect_equal(nrow(correa), 18)
  expect_equal(ncol(correa), 90)

  # correa_batter <- scrape_statcast_savant_batter(start_date = "2016-04-15",
  #                                                end_date = "2016-04-15",
  #                                                batterid = 621043)
  #
  # expect_identical(correa, correa_batter)

  noah <- scrape_statcast_savant(start_date = "2016-04-06",
                                 end_date = "2016-04-15",
                                 playerid = 592789,
                                 player_type = 'pitcher')
  expect_equal(nrow(noah), 99)
  expect_equal(ncol(noah), 90)

  daily <- scrape_statcast_savant(start_date = "2016-04-06",
                                  end_date = "2016-04-06")

  expect_equal(nrow(daily), 3846)
  expect_equal(ncol(daily), 90)

  expect_error(scrape_statcast_savant(start_date = "1970-01-01"), "limited to the 2008")

  expect_equal(nrow(playerid_lookup("Garcia", "Karim")), 1)
  expect_equal(nrow(playerid_lookup("Baumer", "Ben")), 0)
})

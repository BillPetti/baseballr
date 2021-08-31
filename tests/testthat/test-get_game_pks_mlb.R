context("MLB Get Game Packs")

cols <- c(
  "game_pk", "link", "gameType", "season", "gameDate", 
  "officialDate", "isTie", "gameNumber", "publicFacing", 
  "doubleHeader", "gamedayType", "tiebreaker",
  "calendarEventID", "seasonDisplay", "dayNight", 
  "scheduledInnings", "reverseHomeAwayStatus", 
  "inningBreakLength", "gamesInSeries", "seriesGameNumber", 
  "seriesDescription", "recordSource", "ifNecessary",
  "ifNecessaryDescription", "status.abstractGameState", 
  "status.codedGameState", "status.detailedState", 
  "status.statusCode", "status.startTimeTBD", 
  "status.abstractGameCode", "teams.away.score", 
  "teams.away.isWinner", "teams.away.splitSquad",
  "teams.away.seriesNumber", "teams.away.leagueRecord.wins",
  "teams.away.leagueRecord.losses", "teams.away.leagueRecord.pct",
  "teams.away.team.id", "teams.away.team.name", "teams.away.team.link", 
  "teams.home.score", "teams.home.isWinner", "teams.home.splitSquad", 
  "teams.home.seriesNumber", "teams.home.leagueRecord.wins", 
  "teams.home.leagueRecord.losses", "teams.home.leagueRecord.pct",
  "teams.home.team.id", "teams.home.team.name", "teams.home.team.link",
  "venue.id", "venue.name", "venue.link", "content.link"
)

test_that("MLB Get Game Packs", {
  skip_on_cran()
  
  x <- get_game_pks_mlb("2019-04-29")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})


cols <- c(
  "standings_type",
  "last_updated",
  "team_records_season",
  "team_records_clinch_indicator",
  "team_records_division_rank",
  "team_records_league_rank",
  "team_records_sport_rank",
  "team_records_games_played",
  "team_records_games_back",
  "team_records_wild_card_games_back",
  "team_records_league_games_back",
  "team_records_spring_league_games_back",
  "team_records_sport_games_back",
  "team_records_division_games_back",
  "team_records_conference_games_back",
  "team_records_last_updated",
  "team_records_runs_allowed",
  "team_records_runs_scored",
  "team_records_division_champ",
  "team_records_division_leader",
  "team_records_has_wildcard",
  "team_records_clinched",
  "team_records_elimination_number",
  "team_records_wild_card_elimination_number",
  "team_records_magic_number",
  "team_records_wins",
  "team_records_losses",
  "team_records_run_differential",
  "team_records_winning_percentage",
  "team_records_wild_card_rank",
  "team_records_wild_card_leader",
  "team_records_team_id",
  "team_records_team_name",
  "team_records_team_link",
  "team_records_streak_streak_type",
  "team_records_streak_streak_number",
  "team_records_streak_streak_code",
  "team_records_league_record_wins",
  "team_records_league_record_losses",
  "team_records_league_record_ties",
  "team_records_league_record_pct",
  "team_records_records_split_records",
  "team_records_records_division_records",
  "team_records_records_overall_records",
  "team_records_records_league_records",
  "team_records_records_expected_records",
  "league_id",
  "league_link",
  "division_id",
  "division_link",
  "sport_id",
  "sport_link"
)

test_that("MLB Standings", {
  skip_on_cran()
  
  x <- mlb_standings(league_id=103,season=2021)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})

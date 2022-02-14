
cols <- c(
  "game_pk", "home_team_id", "home_team_name",
  "away_team_id", "away_team_name", "num", 
  "ordinal_num", "home_runs", "home_hits",
  "home_errors", "home_left_on_base", "away_runs", 
  "away_hits", "away_errors", "away_left_on_base",
  "home_team_link", "home_team_season", "home_team_venue_id",
  "home_team_venue_name", "home_team_venue_link", 
  "home_team_team_code", "home_team_file_code",
  "home_team_abbreviation", "home_team_team_name", 
  "home_team_location_name", "home_team_first_year_of_play", 
  "home_team_league_id", "home_team_league_name", 
  "home_team_league_link", "home_team_division_id", 
  "home_team_division_name", "home_team_division_link",
  "home_team_sport_id", "home_team_sport_link",
  "home_team_sport_name", "home_team_short_name",
  "home_team_record_games_played", 
  "home_team_record_wild_card_games_back",
  "home_team_record_league_games_back", 
  "home_team_record_spring_league_games_back", 
  "home_team_record_sport_games_back", 
  "home_team_record_division_games_back",
  "home_team_record_conference_games_back", 
  "home_team_record_league_record_wins", 
  "home_team_record_league_record_losses", 
  "home_team_record_league_record_pct", 
  "home_team_record_division_leader",
  "home_team_record_wins", "home_team_record_losses",
  "home_team_record_winning_percentage",
  "home_team_franchise_name", "home_team_club_name",
  "home_team_all_star_status", "home_team_active",
  "away_team_link", "away_team_season",
  "away_team_venue_id", "away_team_venue_name",
  "away_team_venue_link", "away_team_team_code",
  "away_team_file_code", "away_team_abbreviation",
  "away_team_team_name", "away_team_location_name",
  "away_team_first_year_of_play", "away_team_league_id", 
  "away_team_league_name", "away_team_league_link",
  "away_team_division_id", "away_team_division_name", 
  "away_team_division_link", "away_team_sport_id",
  "away_team_sport_link", "away_team_sport_name",
  "away_team_short_name", "away_team_record_games_played",
  "away_team_record_wild_card_games_back", 
  "away_team_record_league_games_back", 
  "away_team_record_spring_league_games_back",
  "away_team_record_sport_games_back", 
  "away_team_record_division_games_back",
  "away_team_record_conference_games_back",
  "away_team_record_league_record_wins",
  "away_team_record_league_record_losses", 
  "away_team_record_league_record_pct", 
  "away_team_record_division_leader", 
  "away_team_record_wins", "away_team_record_losses",
  "away_team_record_winning_percentage", "away_team_franchise_name",
  "away_team_club_name", "away_team_all_star_status", 
  "away_team_active"
)

test_that("MLB Game Linescores", {
  skip_on_cran()
  
  x <- mlb_game_linescore(game_pk = 566001)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})

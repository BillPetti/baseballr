cols <- c(
  "date",
  "total_items",
  "total_events",
  "total_games",
  "total_games_in_progress",
  "game_pk",
  "link",
  "game_type",
  "season",
  "game_date",
  "official_date",
  "is_tie",
  "is_featured_game",
  "game_number",
  "public_facing",
  "double_header",
  "gameday_type",
  "tiebreaker",
  "calendar_event_id",
  "season_display",
  "day_night",
  "description",
  "scheduled_innings",
  "reverse_home_away_status",
  "games_in_series",
  "series_game_number",
  "series_description",
  "record_source",
  "if_necessary",
  "if_necessary_description",
  "status_abstract_game_state",
  "status_coded_game_state",
  "status_detailed_state",
  "status_status_code",
  "status_start_time_tbd",
  "status_abstract_game_code",
  "teams_away_score",
  "teams_away_is_winner",
  "teams_away_split_squad",
  "teams_away_series_number",
  "teams_away_league_record_wins",
  "teams_away_league_record_losses",
  "teams_away_league_record_pct",
  "teams_away_team_id",
  "teams_away_team_name",
  "teams_away_team_link",
  "teams_home_score",
  "teams_home_is_winner",
  "teams_home_split_squad",
  "teams_home_series_number",
  "teams_home_league_record_wins",
  "teams_home_league_record_losses",
  "teams_home_league_record_pct",
  "teams_home_team_id",
  "teams_home_team_name",
  "teams_home_team_link",
  "venue_id",
  "venue_name",
  "venue_link",
  "content_link",
  "inning_break_length",
  "reschedule_date",
  "reschedule_game_date",
  "status_reason",
  "rescheduled_from",
  "rescheduled_from_date",
  "events"
)
test_that("MLB Schedule Postseason", {
  
  skip_on_cran()
  
  x <- mlb_schedule_postseason(season = 2021, sport_id=1)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})

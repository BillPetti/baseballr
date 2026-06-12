# **Get ESPN MLB game rosters**

**Get ESPN MLB game rosters**

## Usage

``` r
espn_mlb_game_rosters(game_id)
```

## Arguments

- game_id:

  Game ID

## Value

A game rosters data frame

|  |  |  |
|----|----|----|
| col_name | types | description |
| athlete_id | integer | Unique ESPN athlete identifier. |
| athlete_uid | character | Athlete uid. |
| athlete_guid | character | Athlete guid. |
| athlete_type | character | Athlete type. |
| sdr | integer | Sdr. |
| first_name | character | First name. |
| last_name | character | Last name. |
| full_name | character | Full name. |
| athlete_display_name | character | Athlete display name (full). |
| short_name | character | Short display name. |
| weight | integer | Weight. |
| display_weight | character | Display weight. |
| height | integer | Height. |
| display_height | character | Display height. |
| age | integer | Age. |
| date_of_birth | character | Date of birth. |
| debut_year | integer | Debut year. |
| birth_place_city | character | Birth place city. |
| birth_place_state | character | Birth place state. |
| birth_place_country | character | Birth place country. |
| slug | character | Slug. |
| headshot_href | character | Headshot href. |
| headshot_alt | character | Headshot alt. |
| athlete_jersey_number | character | Athlete jersey number. |
| position_id | integer | Position id. |
| position_name | character | Position name. |
| position_display_name | character | Position display name. |
| position_abbreviation | character | Position abbreviation. |
| position_leaf | logical | Position leaf. |
| positions_ref | character | Positions ref. |
| positions_id | character | Positions id. |
| positions_name | character | Positions name. |
| positions_display_name | character | Positions display name. |
| positions_abbreviation | character | Positions abbreviation. |
| positions_leaf | logical | Positions leaf. |
| positions_parent_ref | character | Positions parent ref. |
| positions_statistics_ref | character | Positions statistics ref. |
| linked | logical | Linked. |
| years | integer | Years. |
| debut_year_2 | integer | Debut year 2. |
| debut_ref | character | Debut ref. |
| debut_ref_1 | character | Debut ref 1. |
| active | logical | TRUE if the row represents an active record. |
| status_id | integer | Status id. |
| status_name | character | Game status (e.g. 'STATUS_FINAL'). |
| status_type | character | Status type. |
| status_abbreviation | character | Status abbreviation. |
| bats_type | character | Bats type. |
| bats_abbreviation | character | Bats abbreviation. |
| bats_display_value | character | Bats display value. |
| throws_type | character | Throws type. |
| throws_abbreviation | character | Throws abbreviation. |
| throws_display_value | character | Throws display value. |
| starter | logical | TRUE if the player started the game. |
| valid | logical | Valid. |
| display_name | character | Display name. |
| bat_order | integer | Spot in the batting order (1-9; NA if not applicable). |
| record_number | integer | Record number. |
| at_bats | list | At bats. |
| positions | list | Positions. |
| notes | list | Notes. |
| subbed_in_did_sub | logical | Subbed in did sub. |
| subbed_out_did_sub | logical | Subbed out did sub. |
| team_id | integer | Unique ESPN team identifier. |
| team_guid | character | ESPN team GUID. |
| team_uid | character | ESPN universal team identifier (UID). |
| team_sdr | character | Team sdr. |
| team_slug | character | URL-safe team identifier. |
| team_location | character | Team city / location. |
| team_name | character | Team nickname (e.g. 'Yankees'). |
| team_abbreviation | character | Short team abbreviation (e.g. 'NYY'). |
| team_display_name | character | Full team display name (e.g. 'New York Yankees'). |
| team_short_display_name | character | Short team display name. |
| team_color | character | Team primary color (hex, no leading '#'). |
| team_alternate_color | character | Team alternate color (hex). |
| team_is_active | logical | Team is active. |
| is_all_star | logical | Is all star. |
| logo_href | character | Logo href. |
| logo_dark_href | character | Logo dark href. |
| logos_href_2 | character | Logos href 2. |
| logos_href_3 | character | Logos href 3. |
| logos_href_4 | character | Logos href 4. |
| logos_width_4 | integer | Logos width 4. |
| logos_height_4 | integer | Logos height 4. |
| logos_alt_4 | character | Logos alt 4. |
| logos_rel_full_4 | character | Logos rel full 4. |
| logos_rel_primary_logo_on_white_color | character | Logos rel primary logo on white color. |
| logos_last_updated_4 | character | Logos last updated 4. |
| logos_href_5 | character | Logos href 5. |
| logos_width_5 | integer | Logos width 5. |
| logos_height_5 | integer | Logos height 5. |
| logos_alt_5 | character | Logos alt 5. |
| logos_rel_full_5 | character | Logos rel full 5. |
| logos_rel_primary_logo_on_black_color | character | Logos rel primary logo on black color. |
| logos_last_updated_5 | character | Logos last updated 5. |
| logos_href_6 | character | Logos href 6. |
| logos_width_6 | integer | Logos width 6. |
| logos_height_6 | integer | Logos height 6. |
| logos_alt_6 | character | Logos alt 6. |
| logos_rel_full_6 | character | Logos rel full 6. |
| logos_rel_primary_logo_on_primary_color | character | Logos rel primary logo on primary color. |
| logos_last_updated_6 | character | Logos last updated 6. |
| logos_href_7 | character | Logos href 7. |
| logos_width_7 | integer | Logos width 7. |
| logos_height_7 | integer | Logos height 7. |
| logos_alt_7 | character | Logos alt 7. |
| logos_rel_full_7 | character | Logos rel full 7. |
| logos_rel_primary_logo_on_secondary_color | character | Logos rel primary logo on secondary color. |
| logos_last_updated_7 | character | Logos last updated 7. |
| logos_href_8 | character | Logos href 8. |
| logos_width_8 | integer | Logos width 8. |
| logos_height_8 | integer | Logos height 8. |
| logos_alt_8 | character | Logos alt 8. |
| logos_rel_full_8 | character | Logos rel full 8. |
| logos_rel_primary_logo_black | character | Logos rel primary logo black. |
| logos_last_updated_8 | character | Logos last updated 8. |
| logos_href_9 | character | Logos href 9. |
| logos_width_9 | integer | Logos width 9. |
| logos_height_9 | integer | Logos height 9. |
| logos_alt_9 | character | Logos alt 9. |
| logos_rel_full_9 | character | Logos rel full 9. |
| logos_rel_primary_logo_white | character | Logos rel primary logo white. |
| logos_last_updated_9 | character | Logos last updated 9. |
| logos_href_10 | character | Logos href 10. |
| logos_width_10 | integer | Logos width 10. |
| logos_height_10 | integer | Logos height 10. |
| logos_alt_10 | character | Logos alt 10. |
| logos_rel_full_10 | character | Logos rel full 10. |
| logos_rel_secondary_logo_on_white_color | character | Logos rel secondary logo on white color. |
| logos_last_updated_10 | character | Logos last updated 10. |
| logos_href_11 | character | Logos href 11. |
| logos_width_11 | integer | Logos width 11. |
| logos_height_11 | integer | Logos height 11. |
| logos_alt_11 | character | Logos alt 11. |
| logos_rel_full_11 | character | Logos rel full 11. |
| logos_rel_secondary_logo_on_black_color | character | Logos rel secondary logo on black color. |
| logos_last_updated_11 | character | Logos last updated 11. |
| logos_href_12 | character | Logos href 12. |
| logos_width_12 | integer | Logos width 12. |
| logos_height_12 | integer | Logos height 12. |
| logos_alt_12 | character | Logos alt 12. |
| logos_rel_full_12 | character | Logos rel full 12. |
| logos_rel_secondary_logo_on_primary_color | character | Logos rel secondary logo on primary color. |
| logos_last_updated_12 | character | Logos last updated 12. |
| logos_href_13 | character | Logos href 13. |
| logos_width_13 | integer | Logos width 13. |
| logos_height_13 | integer | Logos height 13. |
| logos_alt_13 | character | Logos alt 13. |
| logos_rel_full_13 | character | Logos rel full 13. |
| logos_rel_secondary_logo_on_secondary_color | character | Logos rel secondary logo on secondary color. |
| logos_last_updated_13 | character | Logos last updated 13. |
| logos_href_14 | character | Logos href 14. |
| logos_width_14 | integer | Logos width 14. |
| logos_height_14 | integer | Logos height 14. |
| logos_alt_14 | character | Logos alt 14. |
| logos_rel_full_14 | character | Logos rel full 14. |
| logos_rel_secondary_logo_black | character | Logos rel secondary logo black. |
| logos_last_updated_14 | character | Logos last updated 14. |
| logos_href_15 | character | Logos href 15. |
| logos_width_15 | integer | Logos width 15. |
| logos_height_15 | integer | Logos height 15. |
| logos_alt_15 | character | Logos alt 15. |
| logos_rel_full_15 | character | Logos rel full 15. |
| logos_rel_secondary_logo_white | character | Logos rel secondary logo white. |
| logos_last_updated_15 | character | Logos last updated 15. |
| game_id | integer | Unique ESPN game/event identifier. |
| order | integer | Order. |
| home_away | character | Venue label for the team ('home' or 'away'). |
| winner | logical | Winner. |
| nickname | character | Team nickname. |
| draft_display_text | character | Draft display text. |
| draft_round | integer | Draft round. |
| draft_year | integer | Draft year. |
| draft_selection | integer | Draft selection. |

## See also

Other ESPN MLB Functions:
[`espn_mlb`](https://billpetti.github.io/baseballr/reference/espn_mlb.md),
[`espn_mlb_athletes_index()`](https://billpetti.github.io/baseballr/reference/espn_mlb_athletes_index.md),
[`espn_mlb_award()`](https://billpetti.github.io/baseballr/reference/espn_mlb_award.md),
[`espn_mlb_betting()`](https://billpetti.github.io/baseballr/reference/espn_mlb_betting.md),
[`espn_mlb_calendar()`](https://billpetti.github.io/baseballr/reference/espn_mlb_calendar.md),
[`espn_mlb_coach()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach.md),
[`espn_mlb_coach_record()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach_record.md),
[`espn_mlb_coach_season()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coach_season.md),
[`espn_mlb_coaches()`](https://billpetti.github.io/baseballr/reference/espn_mlb_coaches.md),
[`espn_mlb_conferences()`](https://billpetti.github.io/baseballr/reference/espn_mlb_conferences.md),
[`espn_mlb_draft()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft.md),
[`espn_mlb_draft_athlete_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_athlete_detail.md),
[`espn_mlb_draft_athletes()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_athletes.md),
[`espn_mlb_draft_pick()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_pick.md),
[`espn_mlb_draft_rounds()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_rounds.md),
[`espn_mlb_draft_status()`](https://billpetti.github.io/baseballr/reference/espn_mlb_draft_status.md),
[`espn_mlb_franchise()`](https://billpetti.github.io/baseballr/reference/espn_mlb_franchise.md),
[`espn_mlb_franchises()`](https://billpetti.github.io/baseballr/reference/espn_mlb_franchises.md),
[`espn_mlb_freeagents()`](https://billpetti.github.io/baseballr/reference/espn_mlb_freeagents.md),
[`espn_mlb_futures()`](https://billpetti.github.io/baseballr/reference/espn_mlb_futures.md),
[`espn_mlb_game_all()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_all.md),
[`espn_mlb_game_broadcasts()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_broadcasts.md),
[`espn_mlb_game_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_endpoints.md),
[`espn_mlb_game_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_info.md),
[`espn_mlb_game_odds()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_odds.md),
[`espn_mlb_game_official_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_official_detail.md),
[`espn_mlb_game_officials()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_officials.md),
[`espn_mlb_game_play()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_play.md),
[`espn_mlb_game_play_personnel()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_play_personnel.md),
[`espn_mlb_game_player_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_player_box.md),
[`espn_mlb_game_powerindex()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_powerindex.md),
[`espn_mlb_game_predictor()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_predictor.md),
[`espn_mlb_game_probabilities()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_probabilities.md),
[`espn_mlb_game_probables()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_probables.md),
[`espn_mlb_game_propbets()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_propbets.md),
[`espn_mlb_game_situation()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_situation.md),
[`espn_mlb_game_team_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_leaders.md),
[`espn_mlb_game_team_linescores()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_linescores.md),
[`espn_mlb_game_team_records()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_records.md),
[`espn_mlb_game_team_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_roster.md),
[`espn_mlb_game_team_roster_entry()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_roster_entry.md),
[`espn_mlb_game_team_score()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_score.md),
[`espn_mlb_game_team_statistics()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_team_statistics.md),
[`espn_mlb_injuries()`](https://billpetti.github.io/baseballr/reference/espn_mlb_injuries.md),
[`espn_mlb_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_leaders.md),
[`espn_mlb_news()`](https://billpetti.github.io/baseballr/reference/espn_mlb_news.md),
[`espn_mlb_pbp()`](https://billpetti.github.io/baseballr/reference/espn_mlb_pbp.md),
[`espn_mlb_player_awards()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_awards.md),
[`espn_mlb_player_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_box.md),
[`espn_mlb_player_career_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_career_stats.md),
[`espn_mlb_player_contract()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_contract.md),
[`espn_mlb_player_contracts()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_contracts.md),
[`espn_mlb_player_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_endpoints.md),
[`espn_mlb_player_eventlog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_eventlog.md),
[`espn_mlb_player_eventlog_v2()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_eventlog_v2.md),
[`espn_mlb_player_gamelog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_gamelog.md),
[`espn_mlb_player_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_info.md),
[`espn_mlb_player_overview()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_overview.md),
[`espn_mlb_player_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_seasons.md),
[`espn_mlb_player_splits()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_splits.md),
[`espn_mlb_player_statisticslog()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_statisticslog.md),
[`espn_mlb_player_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_stats.md),
[`espn_mlb_player_stats_v3()`](https://billpetti.github.io/baseballr/reference/espn_mlb_player_stats_v3.md),
[`espn_mlb_position()`](https://billpetti.github.io/baseballr/reference/espn_mlb_position.md),
[`espn_mlb_positions()`](https://billpetti.github.io/baseballr/reference/espn_mlb_positions.md),
[`espn_mlb_powerindex()`](https://billpetti.github.io/baseballr/reference/espn_mlb_powerindex.md),
[`espn_mlb_scoreboard()`](https://billpetti.github.io/baseballr/reference/espn_mlb_scoreboard.md),
[`espn_mlb_season_awards()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_awards.md),
[`espn_mlb_season_draft()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_draft.md),
[`espn_mlb_season_group()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group.md),
[`espn_mlb_season_group_children()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group_children.md),
[`espn_mlb_season_group_teams()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_group_teams.md),
[`espn_mlb_season_groups()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_groups.md),
[`espn_mlb_season_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_info.md),
[`espn_mlb_season_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_leaders.md),
[`espn_mlb_season_ranking()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_ranking.md),
[`espn_mlb_season_rankings()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_rankings.md),
[`espn_mlb_season_type()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_type.md),
[`espn_mlb_season_types()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_types.md),
[`espn_mlb_season_week()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_week.md),
[`espn_mlb_season_weeks()`](https://billpetti.github.io/baseballr/reference/espn_mlb_season_weeks.md),
[`espn_mlb_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_seasons.md),
[`espn_mlb_standings()`](https://billpetti.github.io/baseballr/reference/espn_mlb_standings.md),
[`espn_mlb_team()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team.md),
[`espn_mlb_team_box()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_box.md),
[`espn_mlb_team_current_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_current_roster.md),
[`espn_mlb_team_depthchart()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_depthchart.md),
[`espn_mlb_team_endpoints`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_endpoints.md),
[`espn_mlb_team_injuries()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_injuries.md),
[`espn_mlb_team_leaders()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_leaders.md),
[`espn_mlb_team_news()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_news.md),
[`espn_mlb_team_odds_records()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_odds_records.md),
[`espn_mlb_team_record()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_record.md),
[`espn_mlb_team_record_detail()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_record_detail.md),
[`espn_mlb_team_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_roster.md),
[`espn_mlb_team_schedule()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_schedule.md),
[`espn_mlb_team_season_profile()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_profile.md),
[`espn_mlb_team_season_roster()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_roster.md),
[`espn_mlb_team_season_statistics()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_season_statistics.md),
[`espn_mlb_team_stats()`](https://billpetti.github.io/baseballr/reference/espn_mlb_team_stats.md),
[`espn_mlb_teams()`](https://billpetti.github.io/baseballr/reference/espn_mlb_teams.md),
[`espn_mlb_tournament()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament.md),
[`espn_mlb_tournament_season()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament_season.md),
[`espn_mlb_tournament_seasons()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournament_seasons.md),
[`espn_mlb_tournaments()`](https://billpetti.github.io/baseballr/reference/espn_mlb_tournaments.md),
[`espn_mlb_transactions()`](https://billpetti.github.io/baseballr/reference/espn_mlb_transactions.md),
[`espn_mlb_venues()`](https://billpetti.github.io/baseballr/reference/espn_mlb_venues.md),
[`espn_mlb_week_ranking()`](https://billpetti.github.io/baseballr/reference/espn_mlb_week_ranking.md),
[`espn_mlb_week_rankings()`](https://billpetti.github.io/baseballr/reference/espn_mlb_week_rankings.md),
[`espn_mlb_wp()`](https://billpetti.github.io/baseballr/reference/espn_mlb_wp.md)

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
try(espn_mlb_game_rosters(game_id = 401283399))
#> ✖ 2026-06-12 11:55:33.885255: Invalid arguments or no game roster data for 401283399 available!
#> ✖ Args: game_id = 401283399
#> ✖ Error: The API returned an error, HTTP Response Code 404
#> ── ESPN MLB Game Roster Information from ESPN.com ─── baseballr 2.0.0 ──
#> ℹ Data updated: 2026-06-12 11:55:33 UTC
#> # A tibble: 0 × 0
# }
```

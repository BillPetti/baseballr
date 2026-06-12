# **Get ESPN MLB current team roster**

**Get ESPN MLB current team roster**

## Usage

``` r
espn_mlb_team_current_roster(team_id)
```

## Arguments

- team_id:

  Either numeric or character (YYYY)

## Value

A teams data frame

|  |  |  |
|----|----|----|
| col_name | types | description |
| team_id | integer | Unique ESPN team identifier. |
| team_uid | character | ESPN universal team identifier (UID). |
| team_slug | character | URL-safe team identifier. |
| team_location | character | Team city / location. |
| team_name | character | Team nickname (e.g. 'Yankees'). |
| team_abbreviation | character | Short team abbreviation (e.g. 'NYY'). |
| team_display_name | character | Full team display name (e.g. 'New York Yankees'). |
| team_short_name | character | Team short name. |
| team_color | character | Team primary color (hex, no leading '#'). |
| team_alternate_color | character | Team alternate color (hex). |
| team_is_active | logical | Team is active. |
| franchise..ref | character | Franchise..ref. |
| franchise.id | character | Franchise.id. |
| franchise.uid | character | Franchise.uid. |
| franchise.slug | character | Franchise.slug. |
| franchise.location | character | Franchise.location. |
| franchise.name | character | Franchise.name. |
| franchise.abbreviation | character | Franchise.abbreviation. |
| franchise.displayName | character | Franchise.displayName. |
| franchise.shortDisplayName | character | Franchise.shortDisplayName. |
| franchise.color | character | Franchise.color. |
| franchise.isActive | logical | Franchise.isActive. |
| franchise.venue..ref | character | Franchise.venue..ref. |
| franchise.venue.id | character | Franchise.venue.id. |
| franchise.venue.fullName | character | Franchise.venue.fullName. |
| franchise.venue.shortName | character | Franchise.venue.shortName. |
| franchise.venue.address.city | character | Franchise.venue.address.city. |
| franchise.venue.address.state | character | Franchise.venue.address.state. |
| franchise.venue.address.zipCode | character | Franchise.venue.address.zipCode. |
| franchise.venue.grass | logical | Franchise.venue.grass. |
| franchise.venue.indoor | logical | Franchise.venue.indoor. |
| franchise.venue.images.href | character | Franchise.venue.images.href. |
| franchise.venue.images.width | integer | Franchise.venue.images.width. |
| franchise.venue.images.height | integer | Franchise.venue.images.height. |
| franchise.venue.images.alt | character | Franchise.venue.images.alt. |
| franchise.venue.images.rel | list | Franchise.venue.images.rel. |
| franchise..ref.1 | character | Franchise..ref.1. |
| franchise..ref.2 | character | Franchise..ref.2. |
| standingSummary | character | StandingSummary. |
| logo | character | Logo image URL. |
| logo_dark | character | Dark-mode logo image URL. |
| group_id | integer | Group id. |
| parent_group_id | integer | Parent group id. |
| group_is_conference | logical | Group is conference. |
| conference_id | integer | Conference id. |
| athlete_id | integer | Unique ESPN athlete identifier. |
| athlete_uid | character | Athlete uid. |
| athlete_guid | character | Athlete guid. |
| athlete_type | character | Athlete type. |
| athlete_first_name | character | Athlete first name. |
| athlete_last_name | character | Athlete last name. |
| athlete_full_name | character | Athlete full name. |
| athlete_display_name | character | Athlete display name (full). |
| athlete_short_name | character | Athlete short display name. |
| athlete_weight | integer | Athlete weight. |
| athlete_display_weight | character | Athlete display weight. |
| athlete_height | integer | Athlete height. |
| athlete_display_height | character | Athlete display height. |
| athlete_age | integer | Athlete age. |
| athlete_date_of_birth | character | Athlete date of birth. |
| athlete_slug | character | Athlete slug. |
| athlete_jersey | character | Athlete jersey. |
| athlete_linked | logical | Athlete linked. |
| athlete_active | logical | Athlete active. |
| athlete_positions | list | Athlete positions. |
| athlete_hot_zones | list | Athlete hot zones. |
| athlete_debut_year | integer | Athlete debut year. |
| athlete_nickname | character | Athlete nickname. |
| athlete_middle_name | character | Athlete middle name. |
| athlete_citizenship | character | Athlete citizenship. |
| athlete_alternate_ids_sdr | character | Athlete alternate ids sdr. |
| athlete_birth_place_city | character | Athlete birth place city. |
| athlete_birth_place_state | character | Athlete birth place state. |
| athlete_birth_place_country | character | Athlete birth place country. |
| athlete_position_id | character | Athlete position id. |
| athlete_position_name | character | Athlete fielding position (e.g. 'Shortstop', 'Pitcher'). |
| athlete_position_display_name | character | Athlete position display name. |
| athlete_position_abbreviation | character | Position abbreviation (e.g. 'SS', 'P'). |
| athlete_position_leaf | logical | Athlete position leaf. |
| athlete_experience_years | integer | Athlete experience years. |
| athlete_debut_year_2 | integer | Athlete debut year 2. |
| athlete_status_id | character | Athlete status id. |
| athlete_status_name | character | Athlete status name. |
| athlete_status_type | character | Athlete status type. |
| athlete_status_abbreviation | character | Athlete status abbreviation. |
| athlete_bats_type | character | Athlete bats type. |
| athlete_bats_abbreviation | character | Athlete bats abbreviation. |
| athlete_bats_display_value | character | Athlete bats display value. |
| athlete_throws_type | character | Athlete throws type. |
| athlete_throws_abbreviation | character | Athlete throws abbreviation. |
| athlete_throws_display_value | character | Athlete throws display value. |
| athlete_headshot_href | character | Athlete headshot href. |
| athlete_draft_display_text | character | Athlete draft display text. |
| athlete_draft_round | integer | Athlete draft round. |
| athlete_draft_year | integer | Athlete draft year. |
| athlete_draft_selection | integer | Athlete draft selection. |

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
[`espn_mlb_game_rosters()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_rosters.md),
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
try(espn_mlb_team_current_roster(team_id = 13))
#> ── ESPN MLB Team Current Roster Information from ESPN.com ──────────────
#> ℹ Data updated: 2026-06-12 13:45:22 UTC
#> # A tibble: 282 × 96
#>    team_id team_uid  team_slug team_location team_name team_abbreviation
#>      <int> <chr>     <chr>     <chr>         <chr>     <chr>            
#>  1      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#>  2      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#>  3      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#>  4      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#>  5      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#>  6      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#>  7      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#>  8      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#>  9      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#> 10      13 s:1~l:10… texas-ra… Texas         Rangers   TEX              
#> # ℹ 272 more rows
#> # ℹ 90 more variables: team_display_name <chr>, team_short_name <chr>,
#> #   team_color <chr>, team_alternate_color <chr>, team_is_active <lgl>,
#> #   franchise..ref <chr>, franchise.id <chr>, franchise.uid <chr>,
#> #   franchise.slug <chr>, franchise.location <chr>,
#> #   franchise.name <chr>, franchise.abbreviation <chr>,
#> #   franchise.displayName <chr>, franchise.shortDisplayName <chr>, …
# }
```

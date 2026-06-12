# espn_college_baseball_event_detail.R
# Public NCAA college-baseball shims for ESPN event-detail endpoints.
# Thin wrappers over the league-parameterized helpers in
# espn_baseball_event_helpers.R (the same helpers backing the
# espn_mlb_game_*() family); these fix league = "college-baseball". Return
# shapes are identical to the MLB twins, so the params + @return docs are
# inherited via @inheritParams / @inherit.

# ---------------------------------------------------------------------------
# espn_college_baseball_game_officials
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Officials**
#' @name espn_college_baseball_game_officials
NULL
#' @title
#' **Get ESPN College Baseball Event Officials**
#' @rdname espn_college_baseball_game_officials
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_officials
#' @inherit espn_mlb_game_officials return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble any_of bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_officials(event_id = "401778093"))
#' }
espn_college_baseball_game_officials <- function(event_id, ...) {
  .espn_baseball_event_officials(
    league   = "college-baseball",
    event_id = event_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_broadcasts
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Broadcasts**
#' @name espn_college_baseball_game_broadcasts
NULL
#' @title
#' **Get ESPN College Baseball Event Broadcasts**
#' @rdname espn_college_baseball_game_broadcasts
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_broadcasts
#' @inherit espn_mlb_game_broadcasts return
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble any_of bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_broadcasts(event_id = "401778093"))
#' }
espn_college_baseball_game_broadcasts <- function(event_id, ...) {
  .espn_baseball_event_broadcasts(
    league   = "college-baseball",
    event_id = event_id,
    ...
  )
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_situation
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Situation (Live)**
#' @name espn_college_baseball_game_situation
NULL
#' @title
#' **Get ESPN College Baseball Event Situation (Live)**
#' @rdname espn_college_baseball_game_situation
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_situation
#' @inherit espn_mlb_game_situation return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_situation(event_id = "401778093"))
#' }
espn_college_baseball_game_situation <- function(event_id, ...) {
  .espn_baseball_event_situation(league = "college-baseball", event_id = event_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_team_linescores
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Competitor Linescores (Per-Inning)**
#' @name espn_college_baseball_game_team_linescores
NULL
#' @title
#' **Get ESPN College Baseball Event Competitor Linescores (Per-Inning)**
#' @rdname espn_college_baseball_game_team_linescores
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_team_linescores
#' @inherit espn_mlb_game_team_linescores return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_team_linescores(event_id = "401778093", team_id = "113"))
#' }
espn_college_baseball_game_team_linescores <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_linescores(league = "college-baseball",
                                                  event_id = event_id,
                                                  team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_team_leaders
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Competitor Leaders (Top Performers)**
#' @name espn_college_baseball_game_team_leaders
NULL
#' @title
#' **Get ESPN College Baseball Event Competitor Leaders (Top Performers)**
#' @rdname espn_college_baseball_game_team_leaders
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_team_leaders
#' @inherit espn_mlb_game_team_leaders return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_team_leaders(event_id = "401778093", team_id = "113"))
#' }
espn_college_baseball_game_team_leaders <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_leaders(league = "college-baseball",
                                              event_id = event_id,
                                              team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_team_roster
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Competitor Roster (Game-Day)**
#' @name espn_college_baseball_game_team_roster
NULL
#' @title
#' **Get ESPN College Baseball Event Competitor Roster (Game-Day)**
#' @rdname espn_college_baseball_game_team_roster
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_team_roster
#' @inherit espn_mlb_game_team_roster return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_team_roster(event_id = "401778093", team_id = "113"))
#' }
espn_college_baseball_game_team_roster <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_roster(league = "college-baseball",
                                             event_id = event_id,
                                             team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_team_statistics
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Competitor Team Statistics (Long Format)**
#' @name espn_college_baseball_game_team_statistics
NULL
#' @title
#' **Get ESPN College Baseball Event Competitor Team Statistics (Long Format)**
#' @rdname espn_college_baseball_game_team_statistics
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_team_statistics
#' @inherit espn_mlb_game_team_statistics return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_team_statistics(event_id = "401778093", team_id = "113"))
#' }
espn_college_baseball_game_team_statistics <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_statistics(league = "college-baseball",
                                                 event_id = event_id,
                                                 team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_team_records
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Competitor Records (At-Game Breakdown)**
#' @name espn_college_baseball_game_team_records
NULL
#' @title
#' **Get ESPN College Baseball Event Competitor Records (At-Game Breakdown)**
#' @rdname espn_college_baseball_game_team_records
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_team_records
#' @inherit espn_mlb_game_team_records return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_team_records(event_id = "401778093", team_id = "113"))
#' }
espn_college_baseball_game_team_records <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_records(league = "college-baseball",
                                              event_id = event_id,
                                              team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_player_box
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Player Box Score (Long Format)**
#' @name espn_college_baseball_game_player_box
NULL
#' @title
#' **Get ESPN College Baseball Event Player Box Score (Long Format)**
#' @rdname espn_college_baseball_game_player_box
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_player_box
#' @inherit espn_mlb_game_player_box return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_player_box(event_id = "401778093", team_id = "113",
#'                                             athlete_id = "5102102"))
#' }
espn_college_baseball_game_player_box <- function(event_id, team_id, athlete_id,
                                        stat_type = 0L, ...) {
  .espn_baseball_event_player_box(league = "college-baseball",
                                       event_id = event_id,
                                       team_id = team_id,
                                       athlete_id = athlete_id,
                                       stat_type = stat_type, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_team_roster_entry
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Competitor Roster Entry (Per-Athlete Game-Day Row)**
#' @name espn_college_baseball_game_team_roster_entry
NULL
#' @title
#' **Get ESPN College Baseball Event Competitor Roster Entry (Per-Athlete Game-Day Row)**
#' @rdname espn_college_baseball_game_team_roster_entry
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_team_roster_entry
#' @inherit espn_mlb_game_team_roster_entry return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_team_roster_entry(event_id = "401778093",
#'                                                    team_id = "113",
#'                                                    athlete_id = "5102102"))
#' }
espn_college_baseball_game_team_roster_entry <- function(event_id, team_id,
                                                     athlete_id, ...) {
  .espn_baseball_event_competitor_roster_entry(league = "college-baseball",
                                                   event_id = event_id,
                                                   team_id = team_id,
                                                   athlete_id = athlete_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_play
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Play Detail (Single Play)**
#' @name espn_college_baseball_game_play
NULL
#' @title
#' **Get ESPN College Baseball Event Play Detail (Single Play)**
#' @rdname espn_college_baseball_game_play
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_play
#' @inherit espn_mlb_game_play return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_play(event_id = "401778093", play_id = "4017780931"))
#' }
espn_college_baseball_game_play <- function(event_id, play_id, ...) {
  .espn_baseball_event_play(league = "college-baseball",
                                event_id = event_id,
                                play_id = play_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_play_personnel
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Play Personnel (On-Court Lineup at Play)**
#' @name espn_college_baseball_game_play_personnel
NULL
#' @title
#' **Get ESPN College Baseball Event Play Personnel (On-Court Lineup at Play)**
#' @rdname espn_college_baseball_game_play_personnel
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_play_personnel
#' @inherit espn_mlb_game_play_personnel return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_play_personnel(event_id = "401778093", play_id = "4017780931"))
#' }
espn_college_baseball_game_play_personnel <- function(event_id, play_id, ...) {
  .espn_baseball_event_play_personnel(league = "college-baseball",
                                          event_id = event_id,
                                          play_id = play_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_team_score
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Competitor Score (Single Row)**
#' @name espn_college_baseball_game_team_score
NULL
#' @title
#' **Get ESPN College Baseball Event Competitor Score (Single Row)**
#' @rdname espn_college_baseball_game_team_score
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_team_score
#' @inherit espn_mlb_game_team_score return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_team_score(event_id = "401778093", team_id = "113"))
#' }
espn_college_baseball_game_team_score <- function(event_id, team_id, ...) {
  .espn_baseball_event_competitor_score(league = "college-baseball",
                                            event_id = event_id,
                                            team_id = team_id, ...)
}

# ---------------------------------------------------------------------------
# espn_college_baseball_game_official_detail
# ---------------------------------------------------------------------------

#' **Get ESPN College Baseball Event Official Detail (Single Official)**
#' @name espn_college_baseball_game_official_detail
NULL
#' @title
#' **Get ESPN College Baseball Event Official Detail (Single Official)**
#' @rdname espn_college_baseball_game_official_detail
#' @author Saiem Gilani
#' @inheritParams espn_mlb_game_official_detail
#' @inherit espn_mlb_game_official_detail return
#' @export
#' @family ESPN College Baseball Functions
#' @examples
#' \donttest{
#'   try(espn_college_baseball_game_official_detail(event_id = "401778093", order = 1))
#' }
espn_college_baseball_game_official_detail <- function(event_id, order, ...) {
  .espn_baseball_event_official_detail(league = "college-baseball",
                                           event_id = event_id,
                                           order = order, ...)
}

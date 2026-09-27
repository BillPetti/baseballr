# Loader for the MLB park dimensions table published by
# sportsdataverse/sdv-reference-data to the sportsdataverse-data release tag
# mlb_parks. Schema: that repo's CONTRACT.md ("mlb_park_dimensions"). Reads the
# csv through .groups_release_loader() (R/load_groups.R) with the contract's
# column classes, so venue_id stays character.

.park_dimensions_col_classes <- c(
  league = "character", season = "integer", venue_id = "character",
  venue_name = "character", retro_park_id = "character",
  left_line_ft = "integer", left_ft = "integer", left_center_ft = "integer",
  center_ft = "integer", right_center_ft = "integer", right_ft = "integer",
  right_line_ft = "integer", capacity = "integer", turf_type = "character",
  roof_type = "character", azimuth_deg = "numeric", elevation_ft = "integer",
  latitude = "numeric", longitude = "numeric", notes = "character"
)

#' @title
#' **Load MLB park dimensions by season from the SportsDataverse data repo**
#' @rdname load_mlb_park_dimensions
#' @description One row per MLB venue per season from 2001 on (regular-season,
#'   spring-training, neutral and international sites), with the fence
#'   distances, capacity, surface, roof, orientation and location **as of that
#'   season**, from the MLB Stats API `venues` endpoint. Published as one
#'   season-less file to the `mlb_parks` release tag on the
#'   [sportsdataverse-data releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
#'   by [sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).
#'
#'   The API lags or misses some fence moves; cited corrections (Camden Yards,
#'   Petco Park, T-Mobile Park, Comerica Park, and 2022 at Rate Field and
#'   Progressive Field) are applied and described in `notes`.
#' @inheritParams load_mlb_groups
#' @return A `baseballr_data` tibble:
#'
#'    |col_name        |types     |description                                                                       |
#'    |:---------------|:---------|:---------------------------------------------------------------------------------|
#'    |league          |character |League key (`mlb`).                                                               |
#'    |season          |integer   |Season (calendar year, 2001 on).                                                  |
#'    |venue_id        |character |MLB Stats API venue id (`venue.id` in MLB game feeds and schedules).              |
#'    |venue_name      |character |Venue name as of that season (e.g. PacBell Park 2001-03, Oracle Park 2019-).      |
#'    |retro_park_id   |character |Retrosheet park id (e.g. `BOS07`); `NA` for most spring-training parks.           |
#'    |left_line_ft    |integer   |Feet from home plate to the fence at the left-field pole.                         |
#'    |left_ft         |integer   |Feet to the fence at MLB's left-field marker.                                     |
#'    |left_center_ft  |integer   |Feet to the fence at MLB's left-centre marker.                                    |
#'    |center_ft       |integer   |Feet to the fence in straightaway centre field.                                   |
#'    |right_center_ft |integer   |Feet to the fence at MLB's right-centre marker.                                   |
#'    |right_ft        |integer   |Feet to the fence at MLB's right-field marker.                                    |
#'    |right_line_ft   |integer   |Feet from home plate to the fence at the right-field pole.                        |
#'    |capacity        |integer   |Seating capacity.                                                                 |
#'    |turf_type       |character |`Grass` or `Artificial Turf`.                                                     |
#'    |roof_type       |character |`Open`, `Retractable` or `Dome`.                                                  |
#'    |azimuth_deg     |numeric   |Degrees clockwise from north of the home plate to centre field line (Fenway 45).  |
#'    |elevation_ft    |integer   |Feet above sea level.                                                             |
#'    |latitude        |numeric   |Latitude, decimal degrees.                                                        |
#'    |longitude       |numeric   |Longitude, decimal degrees.                                                       |
#'    |notes           |character |`NA` unless a curated correction applies: what changed, why, and the citation.    |
#'
#' @export
#' @examples
#' \donttest{
#'   try(load_mlb_park_dimensions())
#' }
load_mlb_park_dimensions <- function(..., dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .groups_release_loader("mlb", "park_dimensions",
    "MLB park dimensions from the SportsDataverse data repo",
    dbConnection = dbConnection, tablename = tablename, ...,
    tag = "mlb_parks", cols = .park_dimensions_col_classes)
}

#' Retrieve batter boxscore data for a single game played
#'
#' This function allows a user to retrieve a boxscore of batter statistics for any game played in the PITCHf/x era (2008-current). The function takes a boxscore.xml url as it's only argument and returns boxscore data for both the home and away batters.
#'
#' @param x A boxscore.xml url for a given game from the MLBAM GameDay app data.
#' @keywords MLB, PITCHf/x, Game Day, boxscore, sabermetrics
#' @importFrom XML xmlParse xmlToList
#' @export
#' @examples \dontrun{
#' url_base <- "http://gd2.mlb.com/components/game/mlb/"
#' url <- paste0(url_base, "year_2016/month_05/day_21/gid_2016_05_21_milmlb_nynmlb_1/boxscore.xml")
#' batter_boxscore(url)}

batter_boxscore <- function(x) {
  url <- x
  box <- XML::xmlParse(url)
  xml_data <- XML::xmlToList(box)
  end <- length(xml_data[[3]]) - 5
  x <- seq(1:end)
  home_batters <- lapply(xml_data[[3]][x], function(x)
    as.data.frame.list(x, stringsAsFactors=FALSE)) %>%
    bind_rows()
  home_batters$team <- xml_data[[8]]['home_id']

  a_end <- length(xml_data[[5]]) - 5
  a_x <- seq(1:a_end)
  away_batters <- lapply(xml_data[[5]][a_x], function(x)
    as.data.frame.list(x, stringsAsFactors=FALSE)) %>%
    bind_rows()
  away_batters$team <- xml_data[[8]]['away_id']

  if(!("gidp" %in% colnames(home_batters)))
  {home_batters$gidp <- NA}

  if(!("gidp" %in% colnames(away_batters)))
  {away_batters$gidp <- NA}

  if(!("note" %in% colnames(home_batters)))
  {home_batters$note <- NA}

  if(!("note" %in% colnames(away_batters)))
  {away_batters$note <- NA}

  if(!("go" %in% colnames(home_batters)))
  {home_batters$go <- NA}

  if(!("go" %in% colnames(away_batters)))
  {away_batters$go <- NA}

  batter_col_names <- c("id", "name", "name_display_first_last", "pos", "bo", "ab", "po", "r", "a", "bb", "sac", "t", "sf", "h", "e", "d", "hbp", "so", "hr", "rbi", "lob", "fldg", "sb", "cs", "s_hr", "s_rbi", "s_h", "s_bb", "s_r", "s_so", "avg", "obp", "slg", "ops", "go", "ao", "gidp", "note", "team")

  home_batters <- select_(home_batters, .dots = batter_col_names)
  away_batters <- select_(away_batters, .dots = batter_col_names)

  batters <- rbind(away_batters, home_batters)
  batters
}

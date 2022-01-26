#' @title **Retrieve pitcher boxscore data for a single game played**
#'
#' @description This function allows a user to retrieve a boxscore of pitcher statistics for any game played in the PITCHf/x era (2008-current). The function takes a boxscore.xml url as it's only argument and returns boxscore data for both the home and away batters.
#'
#' @param x A boxscore.xml url for a given game from the MLBAM GameDay app data.
#' @importFrom XML xmlParse xmlToList
#' @export
#' @examples \donttest{
#' # pitchers
#' url_base <- "http://gd2.mlb.com/components/game/mlb/"
#' url <- paste0(url_base,
#'   "year_2016/month_05/day_21/gid_2016_05_21_milmlb_nynmlb_1/boxscore.xml")
#' pitcher_boxscore(url)}

pitcher_boxscore <- function(x) {
  url <- x
  box <- XML::xmlParse(url)
  xml_data <- XML::xmlToList(box)
  end <- length(xml_data[[2]]) - 1
  x <- seq(1:end)
  away_pitchers <- lapply(xml_data[[2]][x], function(x)
    as.data.frame.list(x, stringsAsFactors=FALSE)) %>%

    dplyr::bind_rows()
  away_pitchers$team <- xml_data[[8]][6]


  h_end <- length(xml_data[[4]]) - 1
  h_x <- seq(1:h_end)
  home_pitchers <- lapply(xml_data[[4]][h_x], function(x)
    as.data.frame.list(x, stringsAsFactors=FALSE)) %>%
    dplyr::bind_rows()
  home_pitchers$team <- xml_data[[8]][7]
    bind_rows()
  home_pitchers$team <- xml_data[[8]]['home_id']

  pitcher_col_names <- c("id", "name", "name_display_first_last", "pos", "out", "bf", "er", "r", "h", "so", "hr", "bb", "np", "s", "w", "l", "sv", "bs", "hld", "s_ip", "s_h", "s_r", "s_er", "s_bb", "s_so", "era", "note", "team")

  home_pitchers <- home_pitchers %>% 
    dplyr::select(dplyr::all_of(pitcher_col_names))
  away_pitchers <- away_pitchers %>% 
    dplyr::select(dplyr::all_of(pitcher_col_names))

  pitchers <- dplyr::bind_rows(away_pitchers, home_pitchers)
  return(pitchers)
}

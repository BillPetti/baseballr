#' Create statlines that includecount and rate metrics for players based on Statcast or PITCHf/x pitch-by-pitch data
#'
#' This function allows you to create statlines of statistics for players or groups of players from raw Statcast or PITCHf/x dat.
#'
#' @param df A data frame of statistics that includes, at a minimum, the following columns: events, description, game_date, and type.
#' @param base Tells the function what to use as the population of pitches to use for the statline. Either swings or contact. Defaults to swings.
#' @keywords MLB, woba, Statcast, PITCHf/x, sabermetrics
#' @importFrom dplyr ungroup select everything filter mutate group_by summarise
#' @importFrom tidyr gather
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @export
#' @examples
#' \dontrun{
#' statline_from_statcast(df, base = "contact")
#' }

statline_from_statcast <- function(df, base = "swings") {

  woba_contact <- function(df) {
    df$year <- as.character(df$year)
    guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
    guts_table <- guts_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
    guts_table<- as.data.frame(guts_table)[-(1:2), (1:14)]
    names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")

    for(i in c(2:ncol(guts_table))) {
      guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
    }

    df_join <- left_join(df, guts_table, by = c("year" = "season"))

    df_join$woba <- round((((df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/df$batted_balls),3)

    x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")

    df_join <- df_join[!x]

    df_join
  }

  woba_swings <- function(df) {
    df$year <- as.character(df$year)
    guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
    guts_table <- guts_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
    guts_table<- as.data.frame(guts_table)[-(1:2), (1:14)]
    names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")

    for(i in c(2:ncol(guts_table))) {
      guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
    }

    df_join <- left_join(df, guts_table, by = c("year" = "season"))

    df_join$woba <- round((((df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/df$swings),3)

    x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")

    df_join <- df_join[!x]

    df_join
  }

  df <- df %>%
    dplyr::mutate(hit_type = ifelse(type == "X" & events == "single", 1,
                             ifelse(type == "X" & events== "double", 2,
                                    ifelse(type == "X" & events == "triple", 3, ifelse(type == "X" & events == "home_run", 4, NA)))),
           swing = ifelse(grepl("swinging|into_play|foul", description) & !grepl("bunt", description), 1, 0),
           swinging_strike = ifelse(swing == 1 & grepl("swinging_strike", description), 1, 0))

  year <- substr(max(df$game_date), 1,4)

  if (base == "swings") {
    batted_balls <- df %>%
      dplyr::filter(swing == 1) %>%
      dplyr::group_by(hit_type) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(hit_type = ifelse(is.na(hit_type), "Outs", hit_type)) %>%
      tidyr::spread(key = hit_type, value = n)

    batted_balls <- batted_balls %>%
      mutate('1' = ifelse(1 %in% colnames(batted_balls), `1`, 0),
             '2' = ifelse(2 %in% colnames(batted_balls), `2`, 0),
             '3' = ifelse(3 %in% colnames(batted_balls), `3`, 0),
             '4' = ifelse(4 %in% colnames(batted_balls), `4`, 0),
             Outs = ifelse("Outs" %in% colnames(batted_balls), Outs, 0))

    batted_balls <- batted_balls %>%
      dplyr::select(`1`, `2`, `3`, `4`, Outs)

    names(batted_balls) <- c("X1B", "X2B", "X3B", "HR", "Outs")

    batted_balls <- batted_balls %>%
      dplyr::select(-Outs)

    swing_miss <- df %>%
      dplyr::filter(swing == 1) %>%
      dplyr::summarise(swings = n(), swing_and_miss = sum(swinging_strike),
                swinging_strike_percent = round(swing_and_miss/n(), 3))

    statline <- cbind(batted_balls, swing_miss) %>%
      dplyr::mutate(batted_balls = swings - swing_and_miss, year = year) %>%
      dplyr::select(year, swings, batted_balls, dplyr::everything())

    statline <- statline %>%
      dplyr::mutate(ba = round(sum(X1B, X2B, X3B, HR)/swings, 3), obp = ba, slg = round((X1B + (2*X2B) + (3*X3B) + (4*HR))/swings, 3), ops = obp + slg)

    statline <- woba_swings(statline)

    statline
  }

  else {

    batted_balls <- df %>%
        dplyr::filter(type == "X") %>%
        dplyr::group_by(hit_type) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(hit_type = ifelse(is.na(hit_type), "Outs", hit_type)) %>%
        tidyr::spread(key = hit_type, value = n)

      batted_balls <- batted_balls %>%
        mutate('1' = ifelse(1 %in% colnames(batted_balls), `1`, 0),
               '2' = ifelse(2 %in% colnames(batted_balls), `2`, 0),
               '3' = ifelse(3 %in% colnames(batted_balls), `3`, 0),
               '4' = ifelse(4 %in% colnames(batted_balls), `4`, 0),
               Outs = ifelse("Outs" %in% colnames(batted_balls), Outs, 0))

      batted_balls <- batted_balls %>%
        dplyr::select(`1`, `2`, `3`, `4`, Outs)

      names(batted_balls) <- c("X1B", "X2B", "X3B", "HR", "Outs")

  batted_balls <- batted_balls %>%
    dplyr::mutate(batted_balls = sum(X1B, X2B, X3B, HR, Outs), year = year) %>%
    dplyr::select(-Outs) %>%
    dplyr::select(year, batted_balls, dplyr::everything())

  batted_balls <- batted_balls %>%
    dplyr::mutate(ba = round(sum(X1B, X2B, X3B, HR)/batted_balls, 3), obp = ba, slg = round((X1B + (2*X2B) + (3*X3B) + (4*HR))/batted_balls, 3), ops = obp + slg)

  batted_balls <- woba_contact(batted_balls)

  batted_balls

  }
}

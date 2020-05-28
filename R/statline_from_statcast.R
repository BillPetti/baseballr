#' Create statlines that include count and rate metrics for players based on Statcast or PITCHf/x pitch-by-pitch data
#'
#' This function allows you to create statlines of statistics for players or groups of players from raw Statcast or PITCHf/x data. When calculating wOBA, the most recent year in the data frame is used for weighting.
#'
#' @param df A data frame of statistics that includes, at a minimum, the following columns: events, description, game_date, and type.
#' @param base Tells the function what to use as the population of pitches to use for the statline. Options include "swings", "contact", or "pa". Defaults to "pa".
#' @keywords MLB, woba, Statcast, PITCHf/x, sabermetrics
#' @importFrom tidyr gather replace_na
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @export
#' @examples
#' \dontrun{
#' statline_from_statcast(df, base = "contact")
#' }

statline_from_statcast <- function(df, base = "pa") {

  df$year <- as.character(df$game_year)

  if (!exists("guts_table")) {

    guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
    guts_table <- guts_table %>%
      html_nodes(xpath = '//*[@id="content"]/table') %>%
      html_table(fill = TRUE)
    guts_table<- as.data.frame(guts_table)[-(1:2), (1:14)]
    names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")

    for(i in c(2:ncol(guts_table))) {
      guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
    }

    assign('guts_table', guts_table, envir = .GlobalEnv)
  }

  woba_contact <- function(df) {

    df_join <- left_join(df, guts_table, by = c("year" = "season"))

    df_join$woba <- round((((df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/df$batted_balls),3)

    x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")

    df_join <- df_join[!x]

    df_join
  }

  woba_swings <- function(df) {

    df_join <- left_join(df, guts_table, by = c("year" = "season"))

    df_join$woba <- round((((df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + 	(df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR))/df$swings),3)

    x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")

    df_join <- df_join[!x]

    df_join
  }

  woba_pas <- function(df) {

    df_join <- left_join(df, guts_table, by = c("year" = "season"))

    df_join$woba <- round((((df_join$w1B * df_join$X1B) + (df_join$w2B * df_join$X2B) + (df_join$w3B * df_join$X3B) + (df_join$wHR * df_join$HR) + (df_join$wBB * df_join$BB) + (df_join$wHBP * df_join$HBP))/df$total_pas),3)

    x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")

    df_join <- df_join[!x]

    df_join
  }

  df <- df %>%
    dplyr::mutate(hit_type = ifelse(type == "X" & events == "single", 1,
                                    ifelse(type == "X" & events== "double", 2,
                                           ifelse(type == "X" & events == "triple", 3, ifelse(type == "X" & events == "home_run", 4, NA)))),
                  swing = ifelse(grepl("swinging|into_play|foul", description) & !grepl("bunt", description), 1, 0),
                  swinging_strike = ifelse(swing == 1 & grepl("swinging_strike|foul", description) & !grepl("bunt", description), 1, 0))

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
      dplyr::summarise(swings = n(), swing_and_miss_or_foul = sum(.$swinging_strike),
                       swinging_strike_percent = round(swing_and_miss_or_foul/n(), 3))

    statline <- cbind(batted_balls, swing_miss) %>%
      dplyr::mutate(batted_balls = swings - swing_and_miss_or_foul) %>%
      dplyr::select(swings, batted_balls, dplyr::everything())

    statline <- statline %>%
      dplyr::mutate(ba = round(sum(X1B, X2B, X3B, HR)/swings, 3), obp = ba, slg = round((X1B + (2*X2B) + (3*X3B) + (4*HR))/swings, 3), ops = obp + slg)

    statline$year <- substr(max(df$game_date),1,4)

    statline <- select(statline, year, everything())

    statline <- woba_swings(statline)

    return(statline)

  } else if (base == "pa") {

    batted_balls <- df %>%
      dplyr::mutate(end_of_pa = ifelse(type == 'X', 1, 0)) %>%
      dplyr::filter(end_of_pa == 1) %>%
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

    empty_df <- tibble(X1B = NA, X2B = NA, X3B = NA, HR = NA, Outs = NA)

    batted_balls <- bind_rows(empty_df, batted_balls)

    if (length(batted_balls$X1B) > 1) {

      batted_balls <- batted_balls[-1,]
    }

    walks_ks <- df %>%
      dplyr::mutate(end_of_pa = ifelse(type %in% c('B', 'S') & events != 'null', 1, 0)) %>%
      dplyr::filter(end_of_pa == 1) %>%
      dplyr::mutate(type = ifelse(type == 'B', "BB", "SO")) %>%
      dplyr::mutate(type = ifelse(events == 'hit_by_pitch', 'HBP', type)) %>%
      dplyr::group_by(type) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = type, value = n)

    empty_df <- tibble(BB = NA, HBP = NA, SO = NA)

    walks_ks <- bind_rows(empty_df, walks_ks)

    if (length(walks_ks$BB) > 1) {

      walks_ks <- walks_ks[-1,]
    }

    statline <- bind_cols(batted_balls,
                          walks_ks)

    statline <- cbind(batted_balls, walks_ks)

    statline <- bind_rows(empty_df, statline)

    statline <- statline[-1,]

    statline <- statline %>%
      mutate_all(tidyr::replace_na, 0)

    statline$Outs <- statline$Outs + statline$SO

    statline$total_pas <- with(statline, X1B+X2B+X3B+HR+BB+HBP+Outs)

    statline <- statline %>%
      dplyr::mutate(ba = round(sum(X1B, X2B, X3B, HR)/(X1B+X2B+X3B+HR+Outs), 3),
                    obp = (X1B+X2B+X3B+HR+BB+HBP)/total_pas,
                    slg = round((X1B + (2*X2B) + (3*X3B) + (4*HR))/(X1B+X2B+X3B+HR+Outs), 3),
                    ops = obp + slg)

    statline$year <- substr(max(df$game_date),1,4)

    statline <- select(statline, year, everything())

    statline <- woba_pas(statline)

    statline <- statline %>%
      mutate_at(vars(ba:woba), round, 3) %>%
      mutate_at(vars(ba:woba), ~ifelse(!is.finite(.x), 0, .x))

    return(statline)

  } else {

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
      dplyr::mutate(batted_balls = sum(X1B, X2B, X3B, HR, Outs)) %>%
      dplyr::select(-Outs) %>%
      dplyr::select(batted_balls, dplyr::everything())

    batted_balls <- batted_balls %>%
      dplyr::mutate(ba = round(sum(X1B, X2B, X3B, HR)/batted_balls, 3), obp = ba, slg = round((X1B + (2*X2B) + (3*X3B) + (4*HR))/batted_balls, 3), ops = obp + slg)

    batted_balls$year <- substr(max(df$game_date),1,4)

    batted_balls <- select(batted_balls, year, everything())

    batted_balls <- woba_contact(batted_balls)

    return(batted_balls)
  }
}

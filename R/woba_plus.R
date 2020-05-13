#' Calcuate wOBA and related metrics for any set of data
#'
#' This function allows you to calculate wOBA for any given set of data, provided the right variables are in the data set. The function currently returns both wOBA per plate appearance on wOBA per instance of fair contact.
#'
#' @param df A data frame of statistics that includes, at a minimum, the following columns: uBB (unintentional walks), HBP (Hit By Pitch), X1B (singles), X2B (doubles), X3B (triples), HR (home runs), AB (at-bats), SH (sacrafice hits), SO (strike outs), and season.
#' @keywords MLB, woba, sabermetrics
#' @export
#' @examples
#' \dontrun{
#' woba_plus(df)
#' }

woba_plus <- function(df) {

  if (!exists("guts_table")) {
    df$season <- as.character(df$season)
    guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
    guts_table <- guts_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
    guts_table<- as.data.frame(guts_table)[-(1:2), (1:14)]
    names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
    for(i in c(2:ncol(guts_table))) {
      guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
    }

    assign("guts_table", guts_table, envir = .GlobalEnv)

  }

  df_join <- left_join(df, guts_table, by = "season")

  df_join$wOBA <- round((((df_join$wBB * df_join$uBB) +
                            (df_join$wHBP * df_join$HBP) +
                            (df_join$w1B * df_join$X1B) +
                            (df_join$w2B * df_join$X2B) +
                            (df_join$w3B * df_join$X3B) +
                            (df_join$wHR * df_join$HR))/
                           (df_join$AB + df_join$uBB + df_join$HBP + df_join$SF)),3)

  df_join$wOBA_CON <- round((((df_join$w1B * df_join$X1B) +
                                (df_join$w2B * df_join$X2B) +
                                (df_join$w3B * df_join$X3B) +
                                (df_join$wHR * df_join$HR))/
                               (df_join$AB - df_join$SO)),3)

  df_join <- arrange_(df_join, ~desc(wOBA))
  x <- names(df_join) %in% c("lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  df_join <- df_join[!x]

  return(df_join)

}

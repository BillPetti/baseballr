#' @name metrics
#' @title **Metrics Functions Overview** 
#' 
#' @description
#' \describe{
#'   \item{```fip_plus()```:}{ Calculate FIP and related metrics for any set of data.}
#'   \item{```woba_plus()```}{ Calcuate wOBA and related metrics for any set of data.}
#'   \item{```team_consistency()```}{ Calculate Team-level Consistency.}
#'   \item{```label_statcast_imputed_data()```}{ Label Statcast data as imputed.}
#'   \item{```run_expectancy_code()```}{ Generate run expectancy and related measures from Baseball Savant data.}
#'   \item{```run_expectancy_table()```}{ Generate run expectancy tables from Baseball Savant data.}
#'   \item{```linear_weights_savant()```}{ Generate linear weight values for events using Baseball Savant data.}
#' }
#' @details
#' ### **Calculate Team-level Consistency**
#' ```r
#'   team_consistency(year=2015)
#' ```
#' 
#' ### **Calcuate wOBA and related metrics for any set of data**
#' ```r
#'   woba_plus(df)
#' ```
#' 
#' ### **Label Statcast data as imputed**
#' ```r
#'   statcast_df <- scrape_statcast_savant("2017-05-01", "2017-05-02")
#'   sc_df <- label_statcast_imputed_data(statcast_df)
#'   mean(sc_df$imputed)
#' ```
#' 
#' ### **Generate run expectancy and related measures from Baseball Savant data**
#' ```r
#' run_expectancy_code(df, level = "plate appearances")
#' ```
#' 
#' ### **Generate run expectancy tables from Baseball Savant data**
#' ```r
#'   run_expectancy_table(df, level = "plate appearances")
#' ```
#' 
#' ### **Generate linear weight values for events using Baseball Savant data**
#' ```r
#'   linear_weights_savant(df, level = "plate appearance")
#' ```
#' 
#' 
NULL
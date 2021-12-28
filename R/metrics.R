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
#'   \item{```linear_weights_savant()```}{ Generate linear weight values for events using Baseball Savant data.}
#' }
#' @details
#' ### **Calculate Team-level Consistency**
#' ```r
#'   team_consistency(year=2015)
#' ```
#' ### **Calculate FIP and related metrics for any set of data**
#' ```r
#'   fips_plus(df)
#' ```
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
#'   df <- statcast_search(start_date = "2016-04-06", end_date = "2016-04-15", 
#'                         playerid = 621043, player_type = 'batter') 
#'   run_expectancy_code(df, level = "plate appearances")
#' ```
#' 
#' ### **Generate linear weight values for events using Baseball Savant data**
#' ```r
#'   df <- statcast_search(start_date = "2016-04-06", end_date = "2016-04-15", 
#'                         playerid = 621043, player_type = 'batter') 
#'   df <- run_expectancy_code(df, level = "plate appearances")
#'   linear_weights_savant(df, level = "plate appearance")
#' ```
#' 
#' 
NULL
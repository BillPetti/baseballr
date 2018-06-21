
#' Label Statcast data as imputed
#' 
#' Based on a series of heuristics, this function attempts to 
#' label Statcast data for which the launch angle and speed 
#' have been imputed. 
#' 
#' @param statcast_df A dataframe containing Statcast batted ball data
#' @param impute_file A csv file giving the launch angle, launch speed, bb_type, events fields to label
#' as imputed. if NULL then it's read from the `extdata` folder of the package.
#' @param inverse_precision inverse of how many digits to truncate the launch angle 
#' and speed to for comparison. default is 10000, i.e. keep 4 digits of precision.
#' @keywords MLB, Statcast, sabermetrics
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom utils read.csv
#' @return A copy of the input dataframe with a new column "imputed" appended. imputed
#' is 1 if launch angle and launch speed are likely imputed, 0 otherwise.
#' @export
#' @examples
#' \dontrun{
#' statcast_df = scrape_statcast_savant("2017-05-01", "2017-05-02")
#' statcast_df = label_statcast_imputed_data(statcast_df)
#' mean(statcast_df$imputed)
#' }
label_statcast_imputed_data <- function(statcast_df, impute_file=NULL, inverse_precision=10000) {

  if (is.null(impute_file)) {
    impute_file = system.file("extdata/statcast_impute.csv", package = "baseballr")
  } 
  
  imputed_df = utils::read.csv(impute_file, stringsAsFactors = FALSE)
  
  imputed_df$imputed <- 1
  tmp <- dplyr::left_join(
    statcast_df %>% mutate(ila=as.integer(launch_angle * inverse_precision), 
                           ils=as.integer(launch_speed * inverse_precision)), 
    imputed_df, by=c("ils", "ila", "bb_type", "events"))
  tmp$imputed <- ifelse(is.na(tmp$imputed), 0, 1)
  tmp
}

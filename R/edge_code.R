#' Edge Code
#'
#' This function allows you to classify individual pitches based on the various categories from the Edge% metric. The dataframe passed to the function must include the batter's handedness, the px and pz coordinates from the PITCHf/x system, and the batter's height.
#' @param df A dataframe that, at a minimum, includes the following columns: batter height (b_height), the batter's handedness (stand), vertical location of the pitch (pz), and then horizontal location of the pitch (pz)
#' @param height_var_name The name of the variable in the dataset that includes the batter's height. Defaults to b_height which assumes an height + inch format. If the variable name is "Height" it assumes the variable is already converted to inches (as is the case in some databases)
#' @keywords MLB, sabermetrics, PITCHf/x
#' @export
#' @examples \dontrun{edge_code(df)}

edge_code <- function(df, height_var_name = "b_height") {

  if (height_var_name == "b_height") {
    if (class(df$px) == "factor") {df$px <- as.numeric(levels(df$px))[df$px]}
    if (class(df$pz) == "factor") {df$pz <- as.numeric(levels(df$pz))[df$pz]}
    if (class(df$b_height) == "factor") {df$b_height <- as.numeric(levels(df$b_height))[df$b_height]}
    f <- as.numeric(lapply(strsplit(df$b_height, "-"), function(x) x[1])) * 12
    i <- as.numeric(lapply(strsplit(df$b_height, "-"), function(x) x[2]))
    df$b_height_inch <- f+i
    df$called_pitch <- ifelse(grepl("Called|Ball", df$des2), 1, 0)
    df$called_strike <- ifelse(grepl("Called", df$des2), 1, 0)
    df$swing <- ifelse(grepl("Swinging|Foul|In play", df$des2), 1, 0)
    df$whiff <- ifelse(grepl("Swinging", df$des2), 1, 0)
    LHH <- filter(df, stand == "L")
    RHH <- filter(df, stand == "R")
    LHH$location <- with(LHH, ifelse(!is.na(px) & !is.na(pz) & px > .21 & px < .81 & pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Inside Edge", ifelse(!is.na(px) & !is.na(pz) & px > -1.20 & px < -0.9 & pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Outside Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz > (1.7 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Upper Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz > (.35 + b_height_inch/12 *.229) & pz < (.65 + b_height_inch/12 *.229), "Lower Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz >= (.65 + b_height_inch/12 *.229) & pz <= (1.7 + b_height_inch/12 *.229), "Heart", ifelse(is.na(px) | is.na(pz), NA, "Out of Zone")))))))
    RHH$location <- with(RHH, ifelse(!is.na(px) & !is.na(pz) & px > -1.03 & px < -.43 & pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Inside Edge", ifelse(!is.na(px) & !is.na(pz) & px > .7 & px < 1.00 & pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Outside Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz > (2.3 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Upper Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz > (.92 + b_height_inch/12 *.136) & pz < (1.22 + b_height_inch/12 *.136), "Lower Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz >= (1.22 + b_height_inch/12 *.136) & pz <= (2.30 + b_height_inch/12 *.136), "Heart", ifelse(is.na(px) | is.na(pz), NA, "Out of Zone")))))))
    df_combined <- rbind(LHH, RHH)
    df_combined$Upper_Edge <- with(df_combined, ifelse(location == "Upper Edge", 1, 0))
    df_combined$Lower_Edge <- with(df_combined, ifelse(location == "Lower Edge", 1, 0))
    df_combined$Inside_Edge <- with(df_combined, ifelse(location == "Inside Edge", 1, 0))
    df_combined$Outside_Edge <- with(df_combined, ifelse(location == "Outside Edge", 1, 0))
    df_combined$Heart <- with(df_combined, ifelse(location == "Heart", 1, 0))
    df_combined$OutOfZone <- with(df_combined, ifelse(location == "Out of Zone", 1, 0))
    df_combined
  }

  else {
    if (class(df$px) == "factor") {df$px <- as.numeric(levels(df$px))[df$px]}
    if (class(df$pz) == "factor") {df$pz <- as.numeric(levels(df$pz))[df$pz]}
    df$b_height_inch <- df$Height
    df$called_pitch <- ifelse(grepl("Called|Ball",
                                    df$description,
                                    ignore.case = TRUE), 1, 0)
    df$called_strike <- ifelse(grepl("Called", df$description,
                                     ignore.case = TRUE), 1, 0)
    df$swing <- ifelse(grepl("Swinging|Foul|In play", df$description,
                             ignore.case = TRUE), 1, 0)
    df$whiff <- ifelse(grepl("Swinging", df$description,
                             ignore.case = TRUE), 1, 0)
    LHH <- filter(df, stand == "L")
    RHH <- filter(df, stand == "R")
    LHH$location <- with(LHH, ifelse(!is.na(px) & !is.na(pz) & px > .21 & px < .81 & pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Inside Edge", ifelse(!is.na(px) & !is.na(pz) & px > -1.20 & px < -0.9 & pz > (.35 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Outside Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz > (1.7 + b_height_inch/12 *.229) & pz < (2.0 + b_height_inch/12 *.229), "Upper Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz > (.35 + b_height_inch/12 *.229) & pz < (.65 + b_height_inch/12 *.229), "Lower Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -0.9 & px <= .21 & pz >= (.65 + b_height_inch/12 *.229) & pz <= (1.7 + b_height_inch/12 *.229), "Heart", ifelse(is.na(px) | is.na(pz), NA, "Out of Zone")))))))
    RHH$location <- with(RHH, ifelse(!is.na(px) & !is.na(pz) & px > -1.03 & px < -.43 & pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Inside Edge", ifelse(!is.na(px) & !is.na(pz) & px > .7 & px < 1.00 & pz > (.92 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Outside Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz > (2.3 + b_height_inch/12 *.136) & pz < (2.6 + b_height_inch/12 *.136), "Upper Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz > (.92 + b_height_inch/12 *.136) & pz < (1.22 + b_height_inch/12 *.136), "Lower Edge", ifelse(!is.na(px) & !is.na(pz) & px >= -.43 & px <= .70 & pz >= (1.22 + b_height_inch/12 *.136) & pz <= (2.30 + b_height_inch/12 *.136), "Heart", ifelse(is.na(px) | is.na(pz), NA, "Out of Zone")))))))
    df_combined <- rbind(LHH, RHH)
    df_combined$Upper_Edge <- with(df_combined, ifelse(location == "Upper Edge", 1, 0))
    df_combined$Lower_Edge <- with(df_combined, ifelse(location == "Lower Edge", 1, 0))
    df_combined$Inside_Edge <- with(df_combined, ifelse(location == "Inside Edge", 1, 0))
    df_combined$Outside_Edge <- with(df_combined, ifelse(location == "Outside Edge", 1, 0))
    df_combined$Heart <- with(df_combined, ifelse(location == "Heart", 1, 0))
    df_combined$OutOfZone <- with(df_combined, ifelse(location == "Out of Zone", 1, 0))
    df_combined
    }
}

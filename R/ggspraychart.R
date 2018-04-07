#' Generate spray charts with ggplot2
#'
#' This function allows you to create spray charts with ggplots given a data frame with batted ball location coordinates.
#' @param data A data frame that includes batted ball coordinates. Typically, this coordinates will come from the GameDay xml feed or downloads from baseballsavant.com
#' @param x_value The x coordindate. Typically hc_x.
#' @param y_value The y coordinate. Typically hc_y. You generally need the inverse or negative of the hc_y values, so it is recommended you calculate before plotting.
#' @param fill_value The categorical variable that you want the geom_points to base the fill on. Pass as a string. If left blank, defaults to blue.
#' @param fill_palette An object containing a customer palette to be used with ggplto2::scale_fill_manual.
#' @param fill_legend_title A string containing a custom legend title to be used with ggplto2::scale_fill_manual.
#' @param density Chooses between a 2d density plot or a point plot. Defaults to FALSE.
#' @param bin_size Size of bins used when building a density plot. Defaults to 15.
#' @param point_alpha Alpha value whenever geom_point is used. Deaults to .75. Recommend .3 for density plots. To remove points on density points set use point_alpha = 0.
#' @param frame Variable to use as the frame argument if using gganimate to create animated plots. For density plots be sure your variable is a factor.
#' @keywords MLB, sabermetrics, Statcast, ggplot2
#' @importFrom ggplot2 ggplot geom_point geom_curve geom_segment coord_fixed theme stat_density2d xlim ylim scale_fill_continuous
#' @export
#' @examples
#' \dontrun{
#' ggspraychart(df, x_value = "hc_x", y_value = "-hc_y", fill_value = "events")
#' }

ggspraychart <- function(data, x_value = "hc_x",
                         y_value = "-hc_y", fill_value = NULL,
                         fill_palette = NULL, fill_legend_title = NULL,
                         density = FALSE, bin_size = 15,
                         point_alpha = .75, frame = NULL) {

  if(density == FALSE) {

    if(is.null(fill_value)) {
    plot <- ggplot(data = data, aes_string(x = x_value, y = y_value,
                                           frame = frame)) +
      geom_point(fill = "blue",
                 color = "grey20", alpha = point_alpha,
                 shape = 21, size = 2, stroke = 1) +
      xlim(0,250) +
      ylim(-250, 0) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100,
                 curvature = -.65) +
      geom_segment(x=128, xend = 33, y=-208, yend = -100) +
      geom_segment(x=128, xend = 223, y=-208, yend = -100) +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156,
                 curvature = -.65, linetype = "dotted") +
      coord_fixed() +
      theme_battedball_grey() +
      theme(plot.title = element_text(face = "bold", size = 25),
            strip.text.x = element_text(face = "bold", size = 20),
            legend.position = "bottom", legend.text = element_text(size = 17),
            legend.title = element_text(size = 17))

  } else {
    plot <- ggplot(data = data, aes_string(x = x_value, y = y_value, frame = frame)) +
      geom_point(aes_string(fill = fill_value),
                 color = "grey20", alpha = point_alpha, shape = 21, size = 2, stroke = 1) +
      xlim(0,250) +
      ylim(-250, 0) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100,
               curvature = -.65) +
      geom_segment(x=128, xend = 33, y=-208, yend = -100) +
      geom_segment(x=128, xend = 223, y=-208, yend = -100) +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156,
               curvature = -.65, linetype = "dotted") +
      scale_fill_manual(values = fill_palette, fill_legend_title) +
      coord_fixed() +
      theme_battedball_grey() +
      theme(plot.title = element_text(face = "bold", size = 25),
            strip.text.x = element_text(face = "bold", size = 20),
            legend.position = "bottom", legend.text = element_text(size = 17),
            legend.title = element_text(size = 17))
  }
  } else {
    plot <- ggplot(data = data, aes_string(x = x_value, y = y_value, frame = frame)) +
      stat_density2d(aes(fill = ..level..), contour = TRUE,
                     geom = "polygon", alpha = .3, bins = bin_size) +
      stat_density2d(color = "grey20", alpha = .6, bins = bin_size) +
      geom_point(alpha = point_alpha) +
      scale_fill_continuous(low = "#006BA4", high = "#C85200",
                            guide = FALSE) +
      xlim(0,250) +
      ylim(-250, 0) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100,
                 curvature = -.65) +
      geom_segment(x=128, xend = 33, y=-208, yend = -100) +
      geom_segment(x=128, xend = 223, y=-208, yend = -100) +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156,
                 curvature = -.65, linetype = "dotted") +
      coord_fixed() +
      theme_battedball_grey() +
      theme(plot.title = element_text(face = "bold", size = 25),
            strip.text.x = element_text(face = "bold", size = 20),
            legend.position = "bottom", legend.text = element_text(size = 17),
            legend.title = element_text(size = 17))
  }

  return(plot)
}

theme_battedball_grey <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(face = "bold", size = 14),
      panel.grid = element_blank()
    )
}

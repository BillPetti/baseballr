#' @title **Plot pitch locations with the strike zone with ggplot2**
#' @description
#' Plots pitch locations from the catcher's perspective with the batter's
#' strike zone overlaid, from Statcast pitch data as returned by
#' [statcast_search()] or [mlb_pbp()] (the latter via its
#' `pitchData.coordinates.*` columns renamed to `plate_x` / `plate_z`).
#' Colors default to Baseball Savant's pitch-type palette via
#' [statcast_pitch_colors()].
#' @param data A data frame of pitch data containing plate-crossing
#'   coordinates and strike-zone bounds.
#' @param x_value Column with the horizontal plate-crossing coordinate in
#'   feet (catcher's perspective). Defaults to `"plate_x"`.
#' @param y_value Column with the vertical plate-crossing coordinate in feet.
#'   Defaults to `"plate_z"`.
#' @param color_value Categorical column to color points by, as a string.
#'   Defaults to `"pitch_type"`, colored with the Savant palette; pass another
#'   column (or `NULL` for uncolored points) to override.
#' @param sz_top Column with the batter's strike-zone top (feet). Defaults to
#'   `"sz_top"`; the zone rectangle uses the column means.
#' @param sz_bot Column with the batter's strike-zone bottom (feet). Defaults
#'   to `"sz_bot"`.
#' @param point_alpha Alpha for the pitch points. Defaults to `0.6`.
#' @param point_size Size of the pitch points. Defaults to `2`.
#' @return A ggplot2 object: pitch locations from the catcher's perspective
#'   with the strike zone drawn as a rectangle.
#' @importFrom ggplot2 ggplot aes geom_point geom_rect coord_fixed theme_bw
#'   scale_color_manual labs .data
#' @export
#' @examples
#' \donttest{
#'   try({
#'     pitches <- statcast_search("2024-06-01", "2024-06-01")
#'     ggpitchzone(pitches)
#'   })
#' }
ggpitchzone <- function(data,
                        x_value = "plate_x",
                        y_value = "plate_z",
                        color_value = "pitch_type",
                        sz_top = "sz_top",
                        sz_bot = "sz_bot",
                        point_alpha = 0.6,
                        point_size = 2) {

  # Home plate is 17 inches wide; the rulebook zone spans +/- 8.5in = 0.708 ft.
  zone_half_width <- 17 / 2 / 12
  top <- mean(data[[sz_top]], na.rm = TRUE)
  bot <- mean(data[[sz_bot]], na.rm = TRUE)

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x_value]], y = .data[[y_value]])
  ) +
    ggplot2::geom_rect(
      xmin = -zone_half_width, xmax = zone_half_width,
      ymin = bot, ymax = top,
      fill = NA, color = "grey20", linewidth = 0.8
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Horizontal location (ft, catcher's view)",
      y = "Vertical location (ft)"
    )

  if (!is.null(color_value)) {
    plot <- plot +
      ggplot2::geom_point(
        ggplot2::aes(color = .data[[color_value]]),
        alpha = point_alpha, size = point_size
      )
    if (identical(color_value, "pitch_type")) {
      pal <- statcast_pitch_colors()
      plot <- plot +
        ggplot2::scale_color_manual(
          values = stats::setNames(pal$color, pal$pitch_type),
          na.value = "grey70", name = "Pitch type"
        )
    }
  } else {
    plot <- plot +
      ggplot2::geom_point(alpha = point_alpha, size = point_size, color = "grey30")
  }

  plot
}

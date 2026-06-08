# **Generate spray charts with ggplot2**

This function allows you to create spray charts with ggplots given a
data frame with batted ball location coordinates.

## Usage

``` r
ggspraychart(
  data,
  x_value = "hc_x",
  y_value = "-hc_y",
  fill_value = NULL,
  fill_palette = NULL,
  fill_legend_title = NULL,
  density = FALSE,
  bin_size = 15,
  point_alpha = 0.75,
  point_size = 2,
  frame = NULL
)
```

## Arguments

- data:

  A data frame that includes batted ball coordinates. Typically, this
  coordinates will come from the GameDay xml feed or downloads from
  baseballsavant.com

- x_value:

  The x coordindate. Typically hc_x.

- y_value:

  The y coordinate. Typically hc_y. You generally need the inverse or
  negative of the hc_y values, so it is recommended you calculate before
  plotting.

- fill_value:

  The categorical variable that you want the geom_points to base the
  fill on. Pass as a string. If left blank, defaults to blue.

- fill_palette:

  An object containing a customer palette to be used with
  ggplot2::scale_fill_manual.

- fill_legend_title:

  A string containing a custom legend title to be used with
  ggplot2::scale_fill_manual.

- density:

  Chooses between a 2d density plot or a point plot. Defaults to FALSE.

- bin_size:

  Size of bins used when building a density plot. Defaults to 15.

- point_alpha:

  Alpha value whenever geom_point is used. Defaults to .75. Recommend .3
  for density plots. To remove points on density points set use
  point_alpha = 0.

- point_size:

  Set the size of geom_point if used.

- frame:

  Variable to use as the frame argument if using gganimate to create
  animated plots. For density plots be sure your variable is a factor.

## Value

A plot of the spraychart for the supplied dataset

## Details

    ggspraychart(df, x_value = "hc_x", y_value = "-hc_y", fill_value = "events")

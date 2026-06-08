# **Edge Percentage Frequency**

This function allows you to calculate the percent of pitches thrown to
different edges of the strike zone for a pitch by pitch data set that
has been coded using the
[`edge_code()`](https://billpetti.github.io/baseballr/reference/edge_code.md)
function.

## Usage

``` r
edge_frequency(df, group = NULL)
```

## Arguments

- df:

  A data frame of pitch by pitch data that has been coded using the
  [`edge_code()`](https://billpetti.github.io/baseballr/reference/edge_code.md)
  function.

- group:

  Character string indicating what column to group the frequency by. For
  example, "pitcher" or "batter". Defaults to NULL, which calculates the
  frequencies across the entire data set.

## Value

Returns a tibble with the additional edge columns necessary for
frequency calculations.

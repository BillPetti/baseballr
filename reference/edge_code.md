# **Edge Code**

This function allows you to classify individual pitches based on the
various categories from the Edge% metric. The dataframe passed to the
function must include the batter's handedness, the px and pz coordinates
from the PITCHf/x system, and the batter's height.

## Usage

``` r
edge_code(df, height_var_name = "b_height")
```

## Arguments

- df:

  A dataframe that, at a minimum, includes the following columns: batter
  height (b_height), the batter's handedness (stand), vertical location
  of the pitch (pz), and then horizontal location of the pitch (pz)

- height_var_name:

  The name of the variable in the data set that includes the batter's
  height. Defaults to b_height which assumes an height + inch format. If
  the variable name is "Height" it assumes the variable is already
  converted to inches (as is the case in some databases)

## Value

Returns a tibble with the additional edge columns necessary for
calculations.

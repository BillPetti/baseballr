# **Get, Parse, and Format Retrosheet Event and Roster Files**

This function requires the use of the
[**`Chadwick CLI`**](https://github.com/chadwickbureau/chadwick/releases).
Follow the directions at the repository for installation of the CLI
release for your platform. Specifically from the **`Chadwick CLI`**
tools, this function requires the **`cwevent`** application to be
available from the command line. For unix platform users: the
`retrosheet_data()` function uses the
[`system()`](https://rdrr.io/r/base/system.html) interface under the
hood. For Windows and other platform users: the `retrosheet_data()`
function interacts with the **`cwevent`** application using the
`shell()` interface under the hood.

## Usage

``` r
retrosheet_data(
  path_to_directory = NULL,
  years_to_acquire = most_recent_mlb_season() - 1,
  sequence_years = FALSE
)
```

## Arguments

- path_to_directory:

  (default: NULL) A file path that if set, either:

  1.  creates a new directory, or

  2.  uses the path to an existing directory

- years_to_acquire:

  (format: YYYY) The seasons to collect. Single, multiple, and
  sequential years can be passed. If passing multiple years, enclose in
  a vector (i.e. c(2017,2018)). Defaults to
  [`most_recent_mlb_season()`](https://billpetti.github.io/baseballr/reference/most_recent_mlb_season.md).

- sequence_years:

  (logical, default: FALSE): If the seasons passed in the
  years_to_acquire parameter should be sequenced so that the function
  returns all years including and between the vector passed, set the
  argument to TRUE. Defaults to FALSE.

## Value

If `path_to_directory` is not set (default), the process will return a
named list of tibbles: 'events' and 'rosters' for each season provided
to `years_to_acquire` If `path_to_directory` is set, will also write two
csv files to the unzipped directory: 1) a combined csv of the event data
for a given year and 2) a combined csv of each team's roster for each
year provided to `years_to_acquire`

## Details

    retrosheet_data(path_to_directory = NULL,
                    years_to_acquire =  most_recent_mlb_season()-1,
                    sequence_years = FALSE)

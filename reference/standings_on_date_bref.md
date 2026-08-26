# **(legacy) Scrape MLB Standings on a Given Date**

**(legacy) Scrape MLB Standings on a Given Date**

## Usage

``` r
standings_on_date_bref(date, division, from = FALSE)
```

## Arguments

- date:

  a date object

- division:

  One of AL East, AL Central, AL West, AL Overall, NL East, NL Central,
  NL West, and NL Overall. Which divisions exist depends on the date:
  before 1969 only `AL Overall` / `NL Overall`; 1969-1993 adds
  East/West; 1994 onward adds Central. Requesting a division that did
  not exist for the date errors with the era's available options.

- from:

  a logical indicating whether you want standings up to and including
  the date (FALSE, default) or rather standings for games played after
  the date

## Value

Returns a tibble of MLB standings

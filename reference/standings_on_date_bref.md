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

  One or more of AL East, AL Central, AL West, AL Overall, NL East, NL
  Central, NL West, and NL Overall

- from:

  a logical indicating whether you want standings up to and including
  the date (FALSE, default) or rather standings for games played after
  the date

## Value

Returns a tibble of MLB standings

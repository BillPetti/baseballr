# **Read HTML from Baseball Reference with retry on rate-limit (429)**

Baseball Reference rate-limits (HTTP 429) scrapers that make rapid
sequential requests.
[`xml2::read_html()`](http://xml2.r-lib.org/reference/read_xml.md) has
no built-in retry, so a 429 surfaces as a generic error and the caller's
`tryCatch` swallows it into a misleading "Invalid arguments or no data
available" message (#344).

This helper uses
[`httr2::req_retry()`](https://httr2.r-lib.org/reference/req_retry.html)
to retry 429 / 503 responses with exponential backoff (up to 3 tries),
then passes the response body to
[`xml2::read_html()`](http://xml2.r-lib.org/reference/read_xml.md). A
5-second courtesy delay mirrors the existing NCAA scraper convention.

## Usage

``` r
bref_read_html(url, max_tries = 3L)
```

## Arguments

- url:

  A Baseball Reference URL.

- max_tries:

  Integer. Maximum number of attempts (default 3).

## Value

An XML document as returned by
[`xml2::read_html()`](http://xml2.r-lib.org/reference/read_xml.md).

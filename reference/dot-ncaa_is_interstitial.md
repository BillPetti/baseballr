# **Detect a stats.ncaa.org Akamai interstitial challenge**

Some `stats.ncaa.org` routes (e.g. `/teams/{id}/season_to_date_stats`)
are gated behind Akamai Bot Manager. When challenged, the server returns
HTTP 200 but the body is a short interstitial shell (a `bm-verify`
meta-refresh, an `akamai_validation.html` iframe, or a
`request_quota_reached.html` notice) rather than the data page. A static
request cannot solve the challenge, so we detect the shell and degrade
gracefully instead of silently scraping a page that has no data tables.

## Usage

``` r
.ncaa_is_interstitial(body)
```

## Arguments

- body:

  A character scalar: the response body (HTML) as text.

## Value

`TRUE` when `body` looks like an Akamai interstitial, else `FALSE`.

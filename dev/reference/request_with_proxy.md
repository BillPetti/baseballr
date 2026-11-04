# **Retry an http request (with optional proxy) and rate-limit**

`httr2`-based GET helper used by the NCAA (`stats.ncaa.org`) scrapers.
Pass a `proxy` through `...` to route the request through a proxy, e.g.


    ncaa_roster(team_id = 104, year = 2023,
      proxy = list(url = "http://HOST:PORT", username = "USER", password = "PASS"))

The `stats.ncaa.org` edge (Akamai) rate-limits and IP-bans aggressive
scrapers, so this helper sleeps 5 seconds after every request. Rotate
proxies across calls to spread load.

## Usage

``` r
request_with_proxy(
  url,
  ...,
  headers = .ncaa_headers(),
  proxy = getOption("baseballr.proxy")
)
```

## Arguments

- url:

  Request url

- ...:

  currently unused (kept for backwards compatibility)

- headers:

  A named character vector of request headers. Defaults to
  `.ncaa_headers()` (a modern browser header set that passes the
  `stats.ncaa.org` Akamai edge).

- proxy:

  Optional proxy. Either a URL string (e.g.
  `"http://user:pass@host:port"`) or a list of arguments for
  [`httr2::req_proxy()`](https://httr2.r-lib.org/reference/req_proxy.html)
  (e.g.
  `list(url = "http://host:port", username = "u", password = "p")`).
  Defaults to `getOption("baseballr.proxy")`, so a proxy can be set once
  per session with
  `options(baseballr.proxy = list(url = ..., username = ..., password = ...))`.

## Value

An [httr2::response](https://httr2.r-lib.org/reference/response.html)
object.

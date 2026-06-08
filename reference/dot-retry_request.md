# Perform an HTTP GET request with retry logic (ESPN)

Thin `httr2` wrapper used by the ESPN MLB wrappers. Supports optional
query parameters, custom headers, and proxy routing. Proxy resolution
order:

1.  `proxy` argument (caller-supplied, highest precedence).

2.  `getOption("baseballr.proxy")` (session-level fallback – set once
    with `options(baseballr.proxy = ...)`; ESPN wrappers call
    `.retry_request()` directly without `...`, so per-call overrides are
    not threaded through).

3.  `http_proxy` / `https_proxy` / `no_proxy` env vars (read by libcurl
    automatically when no explicit proxy is supplied).

## Usage

``` r
.retry_request(
  url,
  params = list(),
  headers = NULL,
  timeout = 60,
  proxy = NULL
)
```

## Arguments

- url:

  The URL to request.

- params:

  Named list of query parameters (default: empty list).

- headers:

  Named character vector of headers (default: NULL).

- timeout:

  Timeout in seconds (default: 60).

- proxy:

  Optional proxy (see above). Defaults to
  `getOption("baseballr.proxy")`.

## Value

An [httr2::response](https://httr2.r-lib.org/reference/response.html)
object.

## Details

The `proxy` value accepts a single URL string (`"http://host:port"`,
passed to `httr2::req_proxy(url = )`) or a named list spread as keyword
args into
[`httr2::req_proxy()`](https://httr2.r-lib.org/reference/req_proxy.html)
(`url`, `port`, `username`, `password`, `auth`).

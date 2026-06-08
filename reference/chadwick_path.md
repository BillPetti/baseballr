# Check Chadwick installation

Utility functions to help ensure that Chadwick is set up correctly.

The easiest way for the [Chadwick
CLI](https://github.com/chadwickbureau/chadwick/releases) tools to work
on \*nix systems is to set the `LD_LIBRARY_PATH` environment variable.
Unfortunately this environment variable is not set by default during the
Chadwick installation.

`chadwick_ld_library_path()` checks to find the Chadwick shared
libraries, and then set the `LD_LIBRARY_PATH` environment variable. If
`chadwick_ld_library_path()` returns `TRUE`, the `cwevent` command line
program that
[`retrosheet_data()`](https://billpetti.github.io/baseballr/reference/retrosheet_data.md)
depends on should work.

The other functions documented here are mostly for internal use.

## Usage

``` r
chadwick_path()

chadwick_is_installed()

chadwick_find_lib()

chadwick_set_ld_library_path()

chadwick_ld_library_path()
```

## Value

If Chadwick is not installed `NULL`. If Chadwick is installed, the path
to the `cwevent` binary.

`TRUE` or `FALSE`

Path to the Chadwick shared library.

## See also

[`retrosheet_data()`](https://billpetti.github.io/baseballr/reference/retrosheet_data.md)

## Examples

``` r
chadwick_path()
#> ! cwevent is not installed. Please see https://github.com/chadwickbureau/chadwick/releases for installation instructions. 

chadwick_is_installed()
#> ! cwevent is not installed. Please see https://github.com/chadwickbureau/chadwick/releases for installation instructions. 
#> [1] FALSE

chadwick_find_lib()
#> ! cwevent is not installed. Please see https://github.com/chadwickbureau/chadwick/releases for installation instructions. 
if (FALSE) { # \dontrun{
if (chadwick_ld_library_path()) {
  retrosheet_data(tempdir())
}
} # }
```

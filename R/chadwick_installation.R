#' @title Check Chadwick installation
#' @description
#' Utility functions to help ensure that Chadwick is set up correctly. 
#' @return If Chadwick is not installed `NULL`. If Chadwick is installed, the path to the `cwevent` binary.
#' @export
#' @examples
#' chadwick_path()
#' 

chadwick_path <- function() {
  if (.Platform$OS.type == "windows") {
    cmd <- "where"
  } else {
    cmd <- "which"
  }
  
  path <- tryCatch(
    system2(cmd, "cwevent", stdout = TRUE),
    warning = function(w) {
      message("cwevent is not installed. Please see https://github.com/chadwickbureau/chadwick/releases for installation instructions. ")
    }
  )
  if (!is.null(path) && file.exists(path)) {
    return(dirname(path))
  }
}

#' @rdname chadwick_path
#' @return `TRUE` or `FALSE`
#' @export
#' @examples
#' chadwick_is_installed()
#' 

chadwick_is_installed <- function() {
  path <- chadwick_path()
  !is.null(path) && file.exists(path)
}

#' @rdname chadwick_path
#' @export
#' @return Path to the Chadwick shared library. 
#' @examples
#' chadwick_find_lib()

chadwick_find_lib <- function() {
  if (!is.null(cw_path <- chadwick_path())) {
    system2(
      "find", 
      paste(dirname(cw_path), '-name "libchadwick*"'), stdout = TRUE
    ) |>
      dirname() |>
      unique()
  }
}

#' @rdname chadwick_path
#' @export

chadwick_set_ld_library_path <- function() {
  new_ld_library_path <- paste(
    chadwick_find_lib(), 
    Sys.getenv("LD_LIBRARY_PATH"), 
    sep = ":"
  )
  Sys.setenv(LD_LIBRARY_PATH = new_ld_library_path)
}

#' @rdname chadwick_path
#' @description
#' The easiest way for the [Chadwick CLI](https://github.com/chadwickbureau/chadwick/releases) 
#' tools to work on *nix systems is to
#' set the `LD_LIBRARY_PATH` environment variable. Unfortunately this environment
#' variable is not set by default during the Chadwick installation. 
#' 
#' `chadwick_ld_library_path()` checks to find the Chadwick shared libraries, and then
#' set the `LD_LIBRARY_PATH` environment variable. 
#' If `chadwick_ld_library_path()` returns `TRUE`, the `cwevent` command line program 
#' that \code{\link{retrosheet_data}}
#' depends on should work. 
#' 
#' The other functions documented here are mostly for internal use. 
#' 
#' @export
#' @seealso [retrosheet_data()]
#' @examples
#' \dontrun{
#' if (chadwick_ld_library_path()) {
#'   retrosheet_data(tempdir())
#' }
#' }

chadwick_ld_library_path <- function() {
  old_ld_library_paths <- Sys.getenv("LD_LIBRARY_PATH") |>
    stringr::str_split_1(pattern = ":")
  if (!chadwick_find_lib() %in% old_ld_library_paths) {
    chadwick_set_ld_library_path()
  }
  new_ld_library_paths <- Sys.getenv("LD_LIBRARY_PATH") |>
    stringr::str_split_1(pattern = ":")
  chadwick_find_lib() %in% new_ld_library_paths
}


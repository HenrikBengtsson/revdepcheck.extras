#' The callr Site Library and Its Setup
#'
#' @param lib (character string) Path to the site library used for making
#' the \pkg{callr} package (and its recursive dependencies) resolvable to
#' nested \pkg{callr} subprocesses launched during reverse-dependency
#' checks, regardless of how \pkg{revdepcheck} and \pkg{crancache} narrow
#' `.libPaths()` for those subprocesses. Defaults to a per-platform,
#' per-R-version folder under [tools::R_user_dir()].
#'
#' @return
#' `callr_site_library()` returns the (character string) path, without
#' creating it.
#'
#' @details
#' `revdepcheck` installs each reverse dependency's package dependencies
#' in a \pkg{callr} subprocess that, via \pkg{crancache}, may spawn a
#' *further* nested \pkg{callr} subprocess to refresh the binary cache
#' (`crancache:::update_cache_binaries()` calling `callr::rcmd()`). That
#' nested call needs the \pkg{callr} package itself to be loadable, but
#' `revdepcheck` narrows `.libPaths()` for these subprocesses down to just
#' the per-package check library, which does not contain \pkg{callr}
#' unless the checked package happens to depend on it. This surfaces as:
#'
#' ```
#' Error in loadNamespace(x) : there is no package called 'callr'
#' ```
#'
#' R always re-appends `.Library.site` to `.libPaths()`, so pointing
#' environment variable `R_LIBS_SITE` at a library pre-populated with
#' \pkg{callr} (and its recursive dependencies) makes \pkg{callr}
#' resolvable regardless of how `.libPaths()` is otherwise narrowed.
#'
#' Importantly, `R_LIBS_SITE` has to be set in the environment *before*
#' R starts, because `.Library.site` is fixed once, from the
#' environment, at R's native startup. Setting it from within an
#' already-running R session (e.g. via `Sys.setenv()`, or by loading
#' \file{revdep/revdepcheck.Renviron}) is too late to affect
#' `.Library.site` for that session, even though `Sys.getenv("R_LIBS_SITE")`
#' will then report the new value. Export it in the shell that starts R,
#' e.g. in \file{revdep/revdepcheck.init.sh}:
#'
#' ```sh
#' export R_LIBS_SITE=$(Rscript -e 'cat(revdepcheck.extras::callr_site_library())')
#' ```
#'
#' @seealso [setup_callr_site_library()] for populating this library, and
#' [assert_callr_site_library()] for asserting that it is correctly set up.
#'
#' @importFrom tools R_user_dir
#' @export
callr_site_library <- function() {
  file.path(
    R_user_dir("revdepcheck.extras", which = "data"),
    "callr-site-library",
    sprintf("%s-R%s", R.version$platform, getRversion())
  )
}


#' @param force (logical) If TRUE, all required packages are (re-)installed,
#' even those already found in `lib`.
#'
#' @param repos (character vector) The CRAN-like repositories to use.
#' Defaults to `getOption("repos")`.
#'
#' @param quiet (logical) If TRUE, installation output is suppressed.
#'
#' @return
#' `setup_callr_site_library()` returns `lib` invisibly.
#'
#' @rdname callr_site_library
#'
#' @importFrom utils file_test
#' @importFrom crancache install_packages
#' @export
setup_callr_site_library <- function(lib = callr_site_library(), force = FALSE, repos = getOption("repos"), quiet = FALSE) {
  stopifnot(length(lib) == 1L, is.character(lib), !is.na(lib))

  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  if (!file_test("-d", lib)) {
    stop("Failed to create callr site library: ", sQuote(lib), call. = FALSE)
  }

  pkgs <- callr_required_packages()
  if (!force) {
    pkgs <- setdiff(pkgs, list.files(lib))
  }

  if (length(pkgs) > 0L) {
    message(sprintf("Installing %d package(s) into the callr site library (%s): %s",
                     length(pkgs), sQuote(lib), commaq(pkgs)))
    install_packages(pkgs, dependencies = FALSE, lib = lib, repos = repos, quiet = quiet)
  } else {
    message(sprintf("The callr site library (%s) is already up to date", sQuote(lib)))
  }

  invisible(lib)
}


#' @return
#' `assert_callr_site_library()` returns `lib` invisibly. If `R_LIBS_SITE`
#' was not exported before R started, or if `lib` does not contain the
#' 'callr' package, it stops with an informative error.
#'
#' @rdname callr_site_library
#'
#' @export
assert_callr_site_library <- function(lib = callr_site_library()) {
  stopifnot(length(lib) == 1L, is.character(lib), !is.na(lib))

  if (!(lib %in% .Library.site)) {
    stop(sprintf(paste(
      "The callr site library (%s) is not on .Library.site.",
      "This means environment variable R_LIBS_SITE was not exported",
      "*before* this R process started; setting it from within an",
      "already-running R session (e.g. Sys.setenv(), or loading",
      "revdep/revdepcheck.Renviron) is too late to affect .Library.site.",
      "See help(\"callr_site_library\", package = \"revdepcheck.extras\")",
      "for details. Export it in your shell before starting R, e.g.:\n\n",
      "  export R_LIBS_SITE=%s\n\n",
      "and re-run. If the library does not exist yet, first call",
      "revdepcheck.extras::setup_callr_site_library().", sep = "\n"
    ), sQuote(lib), lib), call. = FALSE)
  }

  if (!utils::file_test("-d", file.path(lib, "callr"))) {
    stop(sprintf(paste(
      "The callr site library (%s) is on .Library.site but does not",
      "contain the 'callr' package. Call",
      "revdepcheck.extras::setup_callr_site_library() to populate it,",
      "then re-run.", sep = "\n"
    ), sQuote(lib)), call. = FALSE)
  }

  invisible(lib)
}


#' @importFrom crancache available_packages
callr_required_packages <- function() {
  found <- "callr"
  frontier <- found
  repeat {
    deps <- required_packages(frontier)
    new <- setdiff(deps, found)
    if (length(new) == 0L) break
    found <- union(found, new)
    frontier <- new
  }
  found
}

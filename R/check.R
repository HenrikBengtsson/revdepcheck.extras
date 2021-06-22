#' Run Reverse Dependency Checks
#'
#' @param bioc (logical) Whether or not reverse dependencies
#' on Bioconductor should be checked.
#'
#' @param timeout (numeric or [base::difftime]) The maximum
#' run-time (in minutes) of 'R CMD check' for one package and
#' version, where "old" and "new" counts as two seperate versions.
#'
#' @return What [revdepcheck::revdep_check()] returns.
#'
#' @details
#' This function does:
#' 1. Gives a note if a `R_CHECK_ENVIRON` file exists
#' 2. Lists any `R_CHECK_*` and `_R_CHECK_*` variable sets
#' 3. Calls `precheck()` if such a function is availble
#' 4. Calls [revdepcheck::revdep_check()]
#' 
#' @importFrom utils file_test
#' @importFrom revdepcheck revdep_check
#' @export
check <- function(bioc = TRUE, timeout = as.numeric(Sys.getenv("R_REVDEPCHECK_TIMEOUT", "60"))) {
  stopifnot(length(bioc) == 1L, is.logical(bioc), !is.na(bioc))
  stopifnot(length(timeout) == 1L, !is.na(timeout), timeout > 0)
  timeout <- as.difftime(timeout, units = "mins")

  assert_repos()

  if (file_test("-f", p <- Sys.getenv("R_CHECK_ENVIRON", "~/.R/check.Renviron"))) {
    cat(sprintf("R CMD check will use env vars from %s\n", sQuote(p)))
    cat(sprintf("To disable, set 'R_CHECK_ENVIRON=false' (a fake pathname)\n"))
  }

  ## Use revdep-specific revdep/cache folder?
  path <- file.path(revdep_this_package(), "revdep", "cache")
  if (file_test("-d", path)) {
    path <- normalizePath(path, mustWork = TRUE)
    Sys.setenv(R_USER_CACHE_DIR = path)
    Sys.setenv(XDG_CACHE_HOME = path)
    cat(sprintf("Using local cache folder: %s\n", sQuote(path)))
  }

  envs <- Sys.getenv()
  envs <- envs[grep("^_?R_CHECK_", names(envs))]
  if (length(envs) > 0L) {
    envs <- sprintf(" %02d. %s=%s", seq_along(envs), names(envs), envs)
    envs <- paste(envs, collapse="\n")
    cat(sprintf("Detected R-specific env vars that may affect R CMD check:\n%s\n", envs))
  }

  if (exists("precheck", mode = "function", envir = .GlobalEnv)) {
    precheck <- get("precheck", mode = "function", envir = .GlobalEnv)
    precheck()
  }
  
  revdep_check(bioc = bioc, num_workers = num_workers(),
               timeout = timeout, quiet = FALSE)
}


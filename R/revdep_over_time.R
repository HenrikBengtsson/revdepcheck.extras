#' Get the Number of Reverse Dependencies on CRAN
#'
#' @param pkgs (character vector) One or more packages.
#'
#' @param dates (POSIXct vector) One or more dates.
#'
#' @param none (integer) The default value if there are no dependencies.
#'
#' @param force (logical) If TRUE, cached values are ignored.
#'
#' @return A data.frame
#'
#' @details
#' This function uses the \pkg{checkpoint} package to query the
#' Microsoft R Application Network (MRAN)
#' [Timemachine](https://mran.microsoft.com/timemachine) for reverse
#' package dependencies at particular dates.
#'
#' @importFrom progressr progressor
#' @importFrom future.apply future_lapply
#' @export
revdep_over_time <- function(pkgs, dates, none = NA_integer_, force = FALSE) {
  cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
  loadCache <- R.cache::loadCache
  saveCache <- R.cache::saveCache
  dirs <- c(.packageName)
  dirs_pkg <- c(dirs, "packages")

  count_revdeps <- function(pkg) {
    key <- list(
      method = "revdep_over_time",
      pkg = pkg,
      repos = getOption("repos")
    )
    if (!force && !is.null(n <- loadCache(key, dirs = dirs_pkg))) return(n)
    pkgs <- cran_revdeps(pkg)
    if (length(pkgs) == 0) return(none)
    n <- length(pkgs)
    saveCache(n, key = key, dirs = dirs_pkg)
    n
  }

  stopifnot(inherits(dates, "Date"))
  stopifnot(is.character(pkgs))
  stopifnot(is.integer(none), length(none) == 1L)

  p <- progressor(length(dates) * length(pkgs))
  stats <- future_lapply(dates, FUN = function(date) {
    mran_repos <- getSnapshotURL(date, online = FALSE)
    repos <- c(CRAN = mran_repos)

    key <- list(
      method = "revdep_over_time",
      pkgs = pkgs,
      repos = repos
    )

    pmsg <- format(date, format = "%F")
    if (!force && !is.null(stats_pkgs <- loadCache(key, dirs = dirs))) {
      p(pmsg, amount = length(pkgs))
      return(stats_pkgs)
    }
    
    oopts <- options(repos = repos)
    on.exit(options(oopts), add = TRUE)
    stats_pkgs <- vapply(pkgs, FUN = function(pkg) {
      on.exit(p(pmsg))
      count_revdeps(pkg)
    }, FUN.VALUE = NA_integer_)

    saveCache(stats_pkgs, key = key, dirs = dirs)

    stats_pkgs
  })

  stats <- unlist(stats, use.names = FALSE)
  stats <- matrix(stats, nrow = length(dates), ncol = length(pkgs), byrow = TRUE)
  colnames(stats) <- pkgs
  stats <- as.data.frame(stats)
  stats <- cbind(date = dates, stats)
  stats
}


#' @param package (character) A package name.
#'
#' @param date (POSIXct) A date.
#'
#' @rdname revdep_over_time
#' @export
cran_revdep_on_date <- function(package, date, force = FALSE) {
  cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
  loadCache <- R.cache::loadCache
  saveCache <- R.cache::saveCache
  
  revdeps <- function(package, ...) {
    dirs <- c(.packageName)
    key <- list(
      method = "cran_revdep_on_date",
      pkg = package,
      repos = getOption("repos")
    )
    if (!force && !is.null(pkgs <- loadCache(key, dirs = dirs))) return(pkgs)
    pkgs <- cran_revdeps(package, ...)
    saveCache(pkgs, key = key, dirs = dirs)
    pkgs
  }

  stopifnot(inherits(date, "Date"))
  stopifnot(is.character(package), length(package) == 1L)

  mran_repos <- getSnapshotURL(date, online = FALSE)
  repos <- c(CRAN = mran_repos)
  oopts <- options(repos = repos)
  on.exit(options(oopts), add = TRUE)
    
  revdeps(package)
}


getSnapshotURL <- function(date, online = FALSE) {
  stopifnot(inherits(date, "Date"))
  
  ## Known time-machine CRAN mirrors
  url_roots <- c(
    MRAN = "https://cran.microsoft.com/snapshot",
    RSPM = "https://packagemanager.rstudio.com/cran"
  )

  mirror <- if (date < "2018-01-01") "MRAN" else "RSPM"
  mirror <- getOption("revdepcheck.extras.snapshot.source", mirror)
  mirror <- match.arg(mirror, choices = names(url_roots))
  url_root <- url_roots[mirror]
  file.path(url_root, date, fsep = "/")
}

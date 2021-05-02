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
#' @importFrom future.apply future_lapply
#' @export
revdep_over_time <- function(pkgs, dates, none = NA_integer_, force = FALSE) {
  cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
  getSnapshotUrl <- import_from("checkpoint", "getSnapshotUrl")
  progressor <- progressr::progressor
  loadCache <- R.cache::loadCache
  saveCache <- R.cache::saveCache
  
  count_revdeps <- function(pkg) {
    dirs <- c(.packageName)
    key <- list(
      method = "revdep_over_time",
      pkg = pkg,
      repos = getOption("repos")
    )
    if (!force && !is.null(n <- loadCache(key, dirs = dirs))) return(n)
    pkgs <- cran_revdeps(pkg)
    if (length(pkgs) == 0) return(none)
    n <- length(pkgs)
    saveCache(n, key = key, dirs = dirs)
    n
  }

  stopifnot(inherits(dates, "Date"))
  stopifnot(is.character(pkgs))
  stopifnot(is.integer(none), length(none) == 1L)

  p <- progressor(length(dates) * length(pkgs))
  stats <- future_lapply(dates, FUN = function(date) {
    mran_repos <- getSnapshotUrl(date, online = FALSE)
    pmsg <- format(date, format = "%F")
    repos <- c(CRAN = mran_repos)
    oopts <- options(repos = repos)
    on.exit(options(oopts), add = TRUE)
    
    vapply(pkgs, FUN = function(pkg) {
      on.exit(p(pmsg))
      count_revdeps(pkg)
    }, FUN.VALUE = NA_integer_)
  })

  stats <- unlist(stats, use.names = FALSE)
  stats <- matrix(stats, nrow = length(dates), ncol = length(pkgs), byrow = TRUE)
  colnames(stats) <- pkgs
  stats <- as.data.frame(stats)
  stats <- cbind(date = dates, stats)
  stats
}



#' @export
cran_revdep_on_date <- function(pkg, date, force = FALSE) {
  cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
  getSnapshotUrl <- import_from("checkpoint", "getSnapshotUrl")
  loadCache <- R.cache::loadCache
  saveCache <- R.cache::saveCache
  
  revdeps <- function(pkg, ...) {
    dirs <- c(.packageName)
    key <- list(
      method = "cran_revdep_on_date",
      pkg = pkg,
      repos = getOption("repos")
    )
    if (!force && !is.null(pkgs <- loadCache(key, dirs = dirs))) return(pkgs)
    pkgs <- cran_revdeps(pkg, ...)
    saveCache(pkgs, key = key, dirs = dirs)
    pkgs
  }

  stopifnot(inherits(date, "Date"))
  stopifnot(is.character(pkg), length(pkg) == 1L)

  mran_repos <- getSnapshotUrl(date, online = FALSE)
  repos <- c(CRAN = mran_repos)
  oopts <- options(repos = repos)
  on.exit(options(oopts), add = TRUE)
    
  revdeps(pkg)
}


#' Query and Assert File Descriptor Limit
#'
#' @param type (character) Query the `"soft"` or `"hard"` limit.
#'
#' @return
#' `ulimit_n()` returns an integer, `+Inf` if unlimited, or `NA_integer_`
#' if file descriptor limits cannot be determined (e.g. on non-Unix systems).
#'
#' @details
#' The soft limit returned by `ulimit_n("soft")` can be overridden by setting
#' R option `revdepcheck.ulimit_n` or environment variable `R_REVDEPCHECK_ULIMIT_N`.
#'
#' @export
ulimit_n <- function(type = c("soft", "hard")) {
  type <- match.arg(type)

  if (type == "soft") {
    val <- getOption("revdepcheck.ulimit_n", NA_integer_)
    if (is.na(val)) {
      env <- Sys.getenv("R_REVDEPCHECK_ULIMIT_N", NA_character_)
      if (!is.na(env)) {
        if (identical(env, "unlimited")) return(Inf)
        val <- suppressWarnings(as.integer(env))
        if (!is.na(val)) return(val)
      }
    } else {
      if (identical(val, "unlimited")) return(Inf)
      val <- suppressWarnings(as.integer(val))
      if (!is.na(val)) return(val)
    }
  }

  if (.Platform$OS.type != "unix") return(NA_integer_)

  flag <- switch(type, soft = "-Sn", hard = "-Hn")
  cmd <- sprintf("ulimit %s 2>/dev/null", flag)
  res <- tryCatch(suppressWarnings(system(cmd, intern = TRUE)), error = function(e) character(0))
  if (length(res) == 0L || is.na(res[1]) || !nzchar(res[1])) return(NA_integer_)
  if (!is.null(attr(res, "status")) && attr(res, "status") != 0L) return(NA_integer_)

  res <- trimws(res[1])
  if (identical(res, "unlimited")) return(Inf)
  val <- suppressWarnings(as.integer(res))
  if (is.na(val)) return(NA_integer_)
  val
}

#' @param nworkers (integer) Number of parallel workers to validate for.
#'
#' @param limit (integer or `+Inf`) The current file descriptor limit.
#' Defaults to `ulimit_n("soft")`.
#'
#' @param fds_per_worker (integer) Minimum number of file descriptors required
#' per worker process (default is `3L`).
#'
#' @param fds_baseline (integer) Baseline number of file descriptors required
#' for the R session and SQLite database (default is `50L`).
#'
#' @return
#' `assert_ulimit_n()` returns `limit` invisibly. If the limit is insufficient,
#' it stops with an informative error.
#'
#' @rdname ulimit_n
#' @export
assert_ulimit_n <- function(nworkers = num_workers(), limit = ulimit_n("soft"), fds_per_worker = 3L, fds_baseline = 50L) {
  stopifnot(length(nworkers) == 1L, is.numeric(nworkers), !is.na(nworkers), nworkers >= 1L)

  if (is.null(limit)) limit <- ulimit_n("soft")
  if (is.na(limit) || is.infinite(limit)) return(invisible(limit))

  needed <- as.integer(ceiling(nworkers * fds_per_worker + fds_baseline))
  if (limit < needed) {
    hard_limit <- ulimit_n("hard")

    fix_str <- if (is.finite(hard_limit) && hard_limit > limit) {
      target_limit <- min(hard_limit, max(needed, 4096L))
      sprintf("- Increase the limit before starting R, e.g. 'ulimit -n %d' or 'ulimit -n $(ulimit -Hn)' (your current hard limit is %d)", target_limit, hard_limit)
    } else if (is.infinite(hard_limit)) {
      target_limit <- max(needed, 4096L)
      sprintf("- Increase the limit before starting R, e.g. 'ulimit -n %d' (or 'ulimit -n unlimited')", target_limit)
    } else {
      "- Increase the file descriptor limit in your system configuration (e.g. /etc/security/limits.conf), or ask your system administrator"
    }

    max_workers <- max(1L, floor((limit - fds_baseline) / fds_per_worker))
    worker_str <- sprintf("- Alternatively, decrease the number of parallel workers, e.g. set environment variable R_REVDEPCHECK_NUM_WORKERS=%d (or option 'revdepcheck.num_workers')", max_workers)

    msg <- sprintf(
      "The file descriptor limit ('ulimit -n') is too low for the requested number of parallel workers:\n - File descriptor limit ('ulimit -n'): %d\n - Number of parallel workers: %d\n - Estimated file descriptors needed: >= %d (~%d per worker + %d baseline for R and SQLite)\nRunning revdep checks with this configuration will fail with \"Too many open files\" (system error 24, EMFILE) as workers are launched.\nTo resolve this:\n %s\n %s",
      limit, nworkers, needed, fds_per_worker, fds_baseline, fix_str, worker_str
    )
    stop(msg, call. = FALSE)
  }

  invisible(limit)
}

library(revdepcheck.extras)

message("Testing ulimit_n() ...")
soft <- ulimit_n("soft")
message("ulimit_n('soft'): ", soft)
stopifnot(is.numeric(soft), length(soft) == 1L)

hard <- ulimit_n("hard")
message("ulimit_n('hard'): ", hard)
stopifnot(is.numeric(hard), length(hard) == 1L)

if (.Platform$OS.type == "unix") {
  stopifnot(is.finite(soft) || is.infinite(soft))
  stopifnot(is.finite(hard) || is.infinite(hard))
  if (is.finite(soft) && is.finite(hard)) {
    stopifnot(soft <= hard)
  }
}

# Override via option
oopt <- options(revdepcheck.ulimit_n = 512L)
stopifnot(identical(ulimit_n("soft"), 512L))
options(revdepcheck.ulimit_n = "unlimited")
stopifnot(identical(ulimit_n("soft"), Inf))
options(oopt)

# Override via env var
Sys.setenv(R_REVDEPCHECK_ULIMIT_N = "256")
stopifnot(identical(ulimit_n("soft"), 256L))
Sys.setenv(R_REVDEPCHECK_ULIMIT_N = "unlimited")
stopifnot(identical(ulimit_n("soft"), Inf))
Sys.unsetenv("R_REVDEPCHECK_ULIMIT_N")

message("Testing assert_ulimit_n() ...")
# Should succeed when limit is sufficient
res <- assert_ulimit_n(nworkers = 4L, limit = 1024L)
stopifnot(identical(res, 1024L))

# Should succeed when limit is Inf or NA
res <- assert_ulimit_n(nworkers = 1000L, limit = Inf)
stopifnot(identical(res, Inf))
res <- assert_ulimit_n(nworkers = 1000L, limit = NA_integer_)
stopifnot(is.na(res))

# Should stop when limit is insufficient
err <- tryCatch({
  assert_ulimit_n(nworkers = 336L, limit = 1024L)
  NULL
}, error = function(e) e)

stopifnot(inherits(err, "error"))
msg <- conditionMessage(err)
message("Caught expected error:\n", msg)
stopifnot(
  grepl("ulimit -n", msg, fixed = TRUE),
  grepl("1024", msg, fixed = TRUE),
  grepl("336", msg, fixed = TRUE),
  grepl("Too many open files", msg, fixed = TRUE),
  grepl("R_REVDEPCHECK_NUM_WORKERS", msg, fixed = TRUE)
)

message("Testing ulimit_n() ... DONE")

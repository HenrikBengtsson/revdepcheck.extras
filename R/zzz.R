# Make functions callable from the command line
#' @importFrom R.utils CmdArgsFunction
run <- R.utils::CmdArgsFunction(run)

.onLoad <- function (libname, pkgname) {
  mirror <- getOption("revdepcheck.extras.snapshot.source", NULL)
  if (is.null(mirror)) {
    mirror <- Sys.getenv("R_REVDEPCHECK_EXTRAS_SNAPSHOT_SOURCE", NA_character_)
    if (!is.na(mirror)) {
      options(revdepcheck.extras.snapshot.source = mirror)
    }
  }
}
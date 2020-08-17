#' Run reverse-package dependency checks from the command line (CLI)
#'
#' @param warn (integer) The warn level of \R option `warn` while running
#' checks.
#'
#' @param args (character string) The command-line arguments.
#'
#' @section Examples:
#' Here are some examples how to call this function from the command-line:
#' ```sh
#' Rscript -e revdepcheck.extras::run
#' Rscript -e revdepcheck.extras::run --reset
#' Rscript -e revdepcheck.extras::run --add some, pkg, another
#' Rscript -e revdepcheck.extras::run --todo
#' Rscript -e revdepcheck.extras::run --preinstall-children
#' Rscript -e revdepcheck.extras::run --preinstall-todo
#' ```
#' 
#' @importFrom utils file_test str
#' @importFrom revdepcheck revdep_check
#' @export
run <- function(warn = 1L, args = base::commandArgs(trailingOnly = TRUE)) {
  stopifnot(length(warn) == 1L, is.numeric(warn), !is.na(warn), warn >= 0L)
  oopts <- options(warn = warn)
  on.exit(options(oopts))
  
  if ("--reset" %in% args) {
    revdepcheck::revdep_reset()
  } else if ("--todo-reset" %in% args) {
    revdep_todo_reset()
    todo()
  } else if ("--todo" %in% args) {
    todo()
  } else if ("--add" %in% args) {
    pos <- which("--add" == args)
    if (pos == length(args)) stop("Missing value for option '--add'")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    revdepcheck::revdep_add(packages = pkgs)
    todo()
  } else if ("--rm" %in% args) {
    pos <- which("--rm" == args)
    if (pos == length(args)) stop("Missing value for option '--rm'")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    revdepcheck::revdep_rm(packages = pkgs)
    todo()
  } else if ("--add-broken" %in% args) {
    revdepcheck::revdep_add_broken()
    todo()
  } else if ("--add-error" %in% args) {
  #  res <- revepcheck::revdep_summary()
    pkgs <- revdep_pkgs_with_status("error")
    str(pkgs)
    revdepcheck::revdep_add(packages = pkgs)
    todo()
  } else if ("--add-all" %in% args) {
    revdep_init()
    pkgs <- revdep_children()
    cran_revdeps <- import_from("revdepcheck", cran_revdeps)
    for (pkg in pkgs) {
      pkgs <- c(pkgs, cran_revdeps(pkg))
    }
    pkgs <- unique(pkgs)
    revdepcheck::revdep_add(packages = pkgs)
    todo()
  } else if ("--add-grandchildren" %in% args) {
    revdep_init()
    pkgs <- NULL
    cran_revdeps <- import_from("revdepcheck", cran_revdeps)
    for (pkg in revdep_children()) {
      pkgs <- c(pkgs, cran_revdeps(pkg))
    }
    pkgs <- unique(pkgs)
    revdepcheck::revdep_add(packages = pkgs)
    todo()
  } else if ("--show-check" %in% args) {
    pos <- which("--show-check" == args)
    if (pos == length(args)) stop("Missing value for option '--show-check")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    for (pkg in pkgs) {
      for (dir in c("old", "new")) {
        path <- file.path("revdep", "checks", pkg, dir, sprintf("%s.Rcheck", pkg))
        if (!utils::file_test("-d", path)) next
        pathname <- file.path(path, "00check.log")
        cat("-----------------------------------------------\n")
        cat(sprintf("%s (%s):\n", pkg, dir))
        cat("-----------------------------------------------\n")
        bfr <- readLines(pathname, warn = FALSE)
        tail <- tail(bfr, n = 20L)
        writeLines(tail)
      }
    }
  } else if ("--list-children" %in% args) {
    pkg <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", cran_revdeps)
    pkgs <- cran_revdeps(pkg)
    cat(sprintf("[n=%d] %s\n", length(pkgs), paste(pkgs, collapse = " ")))
  } else if ("--list-error" %in% args) {
    cat(paste(revdep_pkgs_with_status("error"), collapse = " "), "\n", sep="")
  } else if ("--list-failure" %in% args) {
    cat(paste(revdep_pkgs_with_status("failure"), collapse = " "), "\n", sep="")
  } else if ("--add-error" %in% args) {
    revdepcheck::revdep_add(packages = revdep_pkgs_with_status("error"))
  } else if ("--add-failure" %in% args) {
    revdepcheck::revdep_add(packages = revdep_pkgs_with_status("failure"))
  } else if ("--preinstall-update" %in% args) {
    revdep_preinstall_update()
  } else if ("--preinstall-children" %in% args) {
    pkg <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", cran_revdeps)
    pkgs <- cran_revdeps(pkg)
    revdep_preinstall(pkgs)
  } else if ("--preinstall-error" %in% args) {
    res <- revdepcheck::revdep_summary()
    revdep_preinstall(revdep_pkgs_with_status("error"))
  } else if ("--preinstall-failure" %in% args) {
    res <- revdepcheck::revdep_summary()
    revdep_preinstall(revdep_pkgs_with_status("failure"))
  } else if ("--preinstall-todo" %in% args) {
    todo <- revdep_todo()
    revdep_preinstall(todo$package)
  } else if ("--preinstall" %in% args) {
    pos <- which("--preinstall" == args)
    if (pos == length(args)) stop("Missing value for option '--preinstall'")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    revdep_preinstall(pkgs)
  } else {
    stopifnot(length(args) == 0L)
    check()
    revdepcheck::revdep_report(all = TRUE)
  }
}

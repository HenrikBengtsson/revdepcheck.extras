#!/usr/bin/env Rscript
library(revdepcheck.extras)
options(warn = 1L)

precheck <- function() {
  ## WORKAROUND: Remove checked pkgs that use file links, which otherwise
  ## produce warnings which are promoted to errors by revdepcheck.
  unlink("revdep/checks/aroma.affymetrix", recursive = TRUE)
}

#' @importFrom utils file_test
#' @importFrom revdepcheck revdep_check
#' @export

args <- base::commandArgs(trailingOnly = TRUE)
if ("--reset" %in% args) {
  revdep_reset()
} else if ("--todo-reset" %in% args) {
  revdep_todo_reset()
  todo()
} else if ("--todo" %in% args) {
  todo()
} else if ("--add" %in% args) {
  pos <- which("--add" == args)
  if (pos == length(args)) stop("Missing value for option '--add'")
  pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
  revdep_add(packages = pkgs)
  todo()
} else if ("--rm" %in% args) {
  pos <- which("--rm" == args)
  if (pos == length(args)) stop("Missing value for option '--rm'")
  pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
  revdep_rm(packages = pkgs)
  todo()
} else if ("--add-broken" %in% args) {
  revdep_add_broken()
  todo()
} else if ("--add-error" %in% args) {
#  res <- revepcheck::revdep_summary()
  pkgs <- revdep_pkgs_with_status("error")
  str(pkgs)
  revdep_add(packages = pkgs)
  todo()
} else if ("--add-all" %in% args) {
  revdep_init()
  pkgs <- revdep_children()
  for (pkg in pkgs) {
    pkgs <- c(pkgs, revdepcheck:::cran_revdeps(pkg))
  }
  pkgs <- unique(pkgs)
  revdep_add(packages = pkgs)
  todo()
} else if ("--add-grandchildren" %in% args) {
  revdep_init()
  pkgs <- NULL
  for (pkg in revdep_children()) {
    pkgs <- c(pkgs, revdepcheck:::cran_revdeps(pkg))
  }
  pkgs <- unique(pkgs)
  revdep_add(packages = pkgs)
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
  pkgs <- revdepcheck:::cran_revdeps(pkg)
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
  pkgs <- revdepcheck:::cran_revdeps(pkg)
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

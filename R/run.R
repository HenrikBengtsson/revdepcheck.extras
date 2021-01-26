#' Run reverse-package dependency checks from the command line (CLI)
#'
#' @param \dots Not used.
#'
#' @param warn (integer) The warn level of \R option `warn` while running
#' checks.
#'
#' @param args (character string) The command-line arguments.
#'
#' @section Usage:
#' ```
#' Rscript -e revdepcheck.extras::run --args <options>
#'
#' --help                Display this help page
#' --version             Display version
#'
#' List packages:
#' --list-children       List reverse package dependencies
#' --list-grandchildren  List second-generation reverse package dependencies
#'
#' Populate the 'crancache' database with installable package binaries:
#' --preinstall-todo     Pre-install all packages to be checked
#' --preinstall <pkgs>   Pre-install specified packages
#' --preinstall-update   Populate crancache database ...
#' --preinstall-children Populate crancache database ...
#' --preinstall-error    Populate crancache database ...
#' --preinstall-failure  Populate crancache database ...
#'
#' Add and remove packages to be checked:
#' --reset               Full reset to restart checks from scratch
#' --todo-reset          Empty set of packages to be checked
#' --todo                List packages to be checked
#' --add <pkgs>          Add one or more packages to be checked
#' --add-broken          Add "broken" packages to be rechecked
#' --add-error           Add "errored" packages to be rechecked
#' --add-failure         Add "failed" packages to be rechecked
#' --add-children        Add first-generation reverse package dependencies
#' --add-grandchildren   Add second-generation reverse package dependencies
#' --add-all             Add first and second-generation dependencies
#' --add-updated         Add packages that have been updated since last run
#' --rm <pkgs>           Remove one or more packages to be checked
#'
#' Show results:
#' --show-check <pkg>    Display 'old' and 'new' check results during checks
#' --list-error          List all packages that "errored"
#' --list-failure        List all packages that "failed"
#' --list-updated        List packages that have been updated since last run
#' ```
#'
#' @section Examples:
#' Here are some examples how to call this function from the command-line:
#' ```sh
#' Rscript -e revdepcheck.extras::run --args --help
#' Rscript -e revdepcheck.extras::run --args --version
#' Rscript -e revdepcheck.extras::run --args --reset
#' Rscript -e revdepcheck.extras::run --args --add some, pkg, another
#' Rscript -e revdepcheck.extras::run --args --todo
#' Rscript -e revdepcheck.extras::run --args --preinstall-children
#' Rscript -e revdepcheck.extras::run --args --preinstall-todo
#' ```
#' 
#' @importFrom utils help file_test packageVersion str
#' @importFrom revdepcheck revdep_check
#' @export
run <- function(..., warn = 1L, args = base::commandArgs(trailingOnly = TRUE)) {
  ## To please 'R CMD check'
  repo_version <- NULL
  
  stopifnot(length(warn) == 1L, is.numeric(warn), !is.na(warn), warn >= 0L)
  oopts <- options(warn = warn)
  on.exit(options(oopts))

  if ("--help" %in% args) {
    help("run", package = "revdepcheck.extras", help_type = "text")
  } else if ("--version" %in% args) {
    cat(as.character(packageVersion(.packageName)), "\n", sep = "")
  } else if ("--reset" %in% args) {
    revdepcheck::revdep_reset()
  } else if ("--todo-reset" %in% args) {
    revdep_todo_reset()
    todo(print = TRUE)
  } else if ("--todo" %in% args) {
    todo(print = TRUE)
  } else if ("--add" %in% args) {
    pos <- which("--add" == args)
    if (pos == length(args)) stop("Missing value for option '--add'")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    revdepcheck::revdep_add(packages = pkgs)
    todo(print = TRUE)
  } else if ("--rm" %in% args) {
    pos <- which("--rm" == args)
    if (pos == length(args)) stop("Missing value for option '--rm'")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    revdepcheck::revdep_rm(packages = pkgs)
    todo(print = TRUE)
  } else if ("--add-broken" %in% args) {
    revdepcheck::revdep_add_broken()
    todo(print = TRUE)
  } else if ("--add-error" %in% args) {
    pkgs <- revdep_pkgs_with_status("error")
    revdepcheck::revdep_add(packages = pkgs)
    todo(print = TRUE)
  } else if ("--add-failure" %in% args) {
    pkgs <- revdep_pkgs_with_status("failure")
    revdepcheck::revdep_add(packages = pkgs)
  } else if ("--add-children" %in% args) {
    revdep_init()
    pkgs <- revdep_children()
    pkgs <- unique(pkgs)
    revdepcheck::revdep_add(packages = pkgs)
    todo(print = TRUE)
  } else if ("--add-grandchildren" %in% args) {
    revdep_init()
    pkgs <- NULL
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    for (pkg in revdep_children()) {
      pkgs <- c(pkgs, cran_revdeps(pkg))
    }
    pkgs <- unique(pkgs)
    revdepcheck::revdep_add(packages = pkgs)
    todo(print = TRUE)
  } else if ("--add-updated" %in% args) {
    pkgs <- revdep_readme_packages()
    pkgs <- subset(pkgs, version < repo_version)$package
    if (length(pkgs) > 0) {
      revdepcheck::revdep_add(packages = pkgs)
    } else {
      cat("No packages have been updated since last run\n")
    }
  } else if ("--add-all" %in% args) {
    revdep_init()
    pkgs <- revdep_children()
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    for (pkg in pkgs) {
      pkgs <- c(pkgs, cran_revdeps(pkg))
    }
    pkgs <- unique(pkgs)
    revdepcheck::revdep_add(packages = pkgs)
    todo(print = TRUE)
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
        cat(sprintf("Pathname: %s\n", pathname))
        cat("-----------------------------------------------\n")
        bfr <- readLines(pathname, warn = FALSE)
        tail <- tail(bfr, n = 50L)
        writeLines(tail)
      }
    }
  } else if ("--list-children" %in% args) {
    pkg <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    pkgs <- cran_revdeps(pkg)
    cat(sprintf("[n=%d] %s\n", length(pkgs), paste(pkgs, collapse = " ")))
  } else if ("--list-grandchildren" %in% args) {
    pkgs <- NULL
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    for (pkg in revdep_children()) {
      pkgs <- c(pkgs, cran_revdeps(pkg))
    }
    pkgs <- unique(pkgs)
    cat(sprintf("[n=%d] %s\n", length(pkgs), paste(pkgs, collapse = " ")))
  } else if ("--list-error" %in% args) {
    cat(paste(revdep_pkgs_with_status("error"), collapse = " "), "\n", sep="")
  } else if ("--list-failure" %in% args) {
    cat(paste(revdep_pkgs_with_status("failure"), collapse = " "), "\n", sep="")
  } else if ("--list-updated" %in% args) {
    pkgs <- revdep_readme_packages()
    pkgs <- subset(pkgs, version < repo_version)$package
    if (length(pkgs) > 0) {
      cat(paste(pkgs, collapse = " "), "\n", sep="")
    } else {
      cat("No packages have been updated since last run\n")
    }
  } else if ("--preinstall-update" %in% args) {
    revdep_preinstall_update()
  } else if ("--preinstall-children" %in% args) {
    pkg <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    pkgs <- cran_revdeps(pkg)
    revdep_preinstall(pkgs)
  } else if ("--preinstall-error" %in% args) {
    res <- revdepcheck::revdep_summary()
    revdep_preinstall(revdep_pkgs_with_status("error"))
  } else if ("--preinstall-failure" %in% args) {
    res <- revdepcheck::revdep_summary()
    revdep_preinstall(revdep_pkgs_with_status("failure"))
  } else if ("--preinstall-todo" %in% args) {
    pkgs <- todo(print = FALSE)
    revdep_preinstall(pkgs)
  } else if ("--preinstall" %in% args) {
    pos <- which("--preinstall" == args)
    if (pos == length(args)) stop("Missing value for option '--preinstall'")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    revdep_preinstall(pkgs)
  } else {
    if (length(args) > 0L) {
      stop("Unknown command-line arguments: ",
           paste(sQuote(args), collapse = " "))
    }

    ## Check vignettes by default
    ## Requires:
    ## https://github.com/HenrikBengtsson/revdepcheck/tree/feature/check_args
    if (!nzchar(Sys.getenv("R_REVDEPCHECK_CHECK_ARGS"))) {
      Sys.setenv(R_REVDEPCHECK_CHECK_ARGS = "--no-manual")
    }
    
    check()
    revdepcheck::revdep_report(all = TRUE)
  }
}

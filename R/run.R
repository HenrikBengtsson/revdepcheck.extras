#' Run reverse-package dependency checks from the command line (CLI)
#'
#' @param pkg Path to package or package name.  Default to the package
#' in the current working directory.
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
#' --list-todo           List packages "added" that will be checked next run
#'
#' Populate the 'crancache' database with installable package binaries:
#' --preinstall-todo     Pre-install all packages to be checked
#' --preinstall <pkgs>   Pre-install specified packages
#' --preinstall-children Install all reverse dependencies
#' --preinstall-update   Install packages that have been updated since last run
#' --preinstall-error    Install packages that gave an "error" during checks
#' --preinstall-failure  Install packages that failed to be checked
#'
#' Add and remove packages to be checked:
#' --reset               Full reset to restart checks from scratch
#' --todo-reset          Empty set of packages to be checked
#' --add <pkgs>          Add one or more packages to be checked
#' --add-broken          Add "broken" packages to be rechecked
#' --add-error           Add "errored" but not "failed" packages to be rechecked
#' --add-failure         Add "failed" but not "errored" packages to be rechecked
#' --add-children        Add first-generation reverse package dependencies
#' --add-grandchildren   Add second-generation reverse package dependencies
#' --add-all             Add first and second-generation dependencies
#' --add-updated         Add packages that have been updated since last run
#' --add-new             Add new packages that did not exist at last run
#' --rm <pkgs>           Remove one or more packages to be checked
#'
#' Miscellanous setup tweaks:
#' --init                Create an empty revdep data base
#' --use-tmpdir          Host revdep/{library,check} under $TMPDIR
#'
#' Show results:
#' --show-check <pkg>    Display 'old' and 'new' check results during checks
#' --list-error          List all packages that "errored"
#' --list-failure        List all packages that "failed"
#' --list-updated        List packages that have been updated since last run
#' --list-new            List new packages that did not exist at last run
#' ```
#'
#' @section Examples:
#' Here are some examples how to call this function from the command-line:
#' ```sh
#' Rscript -e revdepcheck.extras::run --args --help
#' Rscript -e revdepcheck.extras::run --args --version
#' Rscript -e revdepcheck.extras::run --args --reset
#' Rscript -e revdepcheck.extras::run --args --add some, pkg, another
#' Rscript -e revdepcheck.extras::run --args --list-todo
#' Rscript -e revdepcheck.extras::run --args --preinstall-children
#' Rscript -e revdepcheck.extras::run --args --preinstall-todo
#'
#' R_REVDEPCHECK_NUM_WORKERS=1 NSLOTS=2 R_REVDEPCHECK_TIMEOUT=120 Rscript -e revdepcheck.extras::run
#' ```
#' 
#' @importFrom utils help file_test packageVersion str
#' @importFrom revdepcheck revdep_check
#' @export
run <- function(pkg = ".", ..., warn = 1L, args = base::commandArgs(trailingOnly = TRUE)) {
  ## To please 'R CMD check'
  repo_version <- NULL

  stopifnot(is.character(pkg), length(pkg) == 1L, !is.na(pkg))
  
  stopifnot(length(warn) == 1L, is.numeric(warn), !is.na(warn), warn >= 0L)
  oopts <- options(warn = warn)
  on.exit(options(oopts))

  if ("--help" %in% args) {
    print(help("run", package = "revdepcheck.extras", help_type = "text"))
    return(invisible())
  } else if ("--version" %in% args) {
    cat(as.character(packageVersion(.packageName)), "\n", sep = "")
    return(invisible())
  }

  assert_repos()

  if ("--init" %in% args) {
    revdep_init(pkg)
  } else if ("--use-tmpdir" %in% args) {
    revdep_use_tmpdir(pkg)
  } else if ("--reset" %in% args) {
    revdepcheck::revdep_reset(pkg)
  } else if ("--todo-reset" %in% args) {
    revdep_todo_reset(pkg)
    todo(print = TRUE)
  } else if ("--list-todo" %in% args) {
    todo(print = TRUE)
  } else if ("--add" %in% args) {
    revdep_init(pkg)
    pos <- which("--add" == args)
    if (pos == length(args)) stop("Missing value for option '--add'")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    revdepcheck::revdep_add(pkg, packages = pkgs)
    todo(print = TRUE)
  } else if ("--rm" %in% args) {
    pos <- which("--rm" == args)
    revdep_init(pkg)
    if (pos == length(args)) stop("Missing value for option '--rm'")
    pkgs <- parse_pkgs(args[seq(from = pos + 1L, to = length(args))])
    revdepcheck::revdep_rm(pkg, packages = pkgs)
    todo(print = TRUE)
  } else if ("--add-broken" %in% args) {
    revdep_init(pkg)
    revdepcheck::revdep_add_broken(pkg)
    todo(print = TRUE)
  } else if ("--add-error" %in% args) {
    revdep_init(pkg)
    pkgs <- revdep_pkgs_with_status(pkg, "error")
    revdepcheck::revdep_add(pkg, packages = pkgs)
    todo(print = TRUE)
  } else if ("--add-failure" %in% args) {
    revdep_init(pkg)
    pkgs <- revdep_pkgs_with_status(pkg, "failure")
    revdepcheck::revdep_add(packages = pkgs)
  } else if ("--add-children" %in% args) {
    revdep_init(pkg)
    pkgs <- revdep_children(pkg)
    revdepcheck::revdep_add(pkg, packages = pkgs)
    todo(print = TRUE)
  } else if ("--add-grandchildren" %in% args) {
    revdep_init(pkg)
    pkgs <- revdep_grandchildren(pkg)
    revdepcheck::revdep_add(pkg, packages = pkgs)
    todo(print = TRUE)
  } else if ("--add-updated" %in% args) {
    revdep_init(pkg)
    pkgs <- revdep_readme_packages()
    pkgs <- subset(pkgs, version < repo_version)$package
    if (length(pkgs) > 0) {
      revdepcheck::revdep_add(pkg, packages = pkgs)
    } else {
      cat("No packages have been updated since last run\n")
    }
  } else if ("--add-new" %in% args) {
    revdep_init(pkg)
    pkgs <- revdep_readme_packages()
    children <- revdepcheck.extras::revdep_children(pkg)
    pkgs <- setdiff(children, pkgs$package)
    if (length(pkgs) > 0) {
      revdepcheck::revdep_add(pkg, packages = pkgs)
    } else {
      cat("No new packages found since last run\n")
    }
  } else if ("--add-all" %in% args) {
    revdep_init(pkg)
    pkgs <- revdep_children(pkg)
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    for (pkg in pkgs) {
      pkgs <- c(pkgs, cran_revdeps(pkg))
    }
    pkgs <- unique(pkgs)
    revdepcheck::revdep_add(pkg, packages = pkgs)
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
    if (identical(pkg, ".")) pkg <- revdep_this_package()
    pkgs <- revdep_children(pkg)
    cat(sprintf("[n=%d] %s\n", length(pkgs), paste(pkgs, collapse = " ")))
  } else if ("--list-grandchildren" %in% args) {
    if (identical(pkg, ".")) pkg <- revdep_this_package()
    pkgs <- revdep_grandchildren(pkg)
    cat(sprintf("[n=%d] %s\n", length(pkgs), paste(pkgs, collapse = " ")))
  } else if ("--list-error" %in% args) {
    cat(paste(revdep_pkgs_with_status(pkg, "error"), collapse = " "), "\n", sep="")
  } else if ("--list-failure" %in% args) {
    cat(paste(revdep_pkgs_with_status(pkg, "failure"), collapse = " "), "\n", sep="")
  } else if ("--list-updated" %in% args) {
    pkgs <- revdep_readme_packages()
    pkgs <- subset(pkgs, version < repo_version)$package
    if (length(pkgs) > 0) {
      cat(paste(pkgs, collapse = " "), "\n", sep="")
    } else {
      cat("No packages have been updated since last run\n")
    }
  } else if ("--list-new" %in% args) {
    pkgs <- revdep_readme_packages()
    children <- revdep_children(pkg)
    pkgs <- setdiff(children, pkgs$package)
    if (length(pkgs) > 0) {
      cat(paste(pkgs, collapse = " "), "\n", sep="")
    } else {
      cat("No new packages found since last run\n")
    }
  } else if ("--preinstall-update" %in% args) {
    revdep_preinstall_update()
  } else if ("--preinstall-children" %in% args) {
    if (identical(pkg, ".")) pkg <- revdep_this_package()
    cran_revdeps <- import_from("revdepcheck", "cran_revdeps")
    pkgs <- cran_revdeps(pkg)
    revdep_preinstall(pkgs)
  } else if ("--preinstall-error" %in% args) {
    res <- revdepcheck::revdep_summary(pkg)
    revdep_preinstall(revdep_pkgs_with_status(pkg, "error"))
  } else if ("--preinstall-failure" %in% args) {
    res <- revdepcheck::revdep_summary(pkg)
    revdep_preinstall(revdep_pkgs_with_status(pkg, "failure"))
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

    status <- import_from("revdepcheck", "status")
    status("SETUP")

    t0 <- Sys.time()
    message(sprintf("Start time: %s", format(t0)))

    ## Check vignettes by default
    ## Requires:
    ## https://github.com/HenrikBengtsson/revdepcheck/tree/feature/check_args
    if (!nzchar(Sys.getenv("R_REVDEPCHECK_CHECK_ARGS"))) {
      Sys.setenv(R_REVDEPCHECK_CHECK_ARGS = "--no-manual")
      ## Assert we're using a 'revdepcheck' version that supports this
      if (!exists("revdep_check_args", mode = "function", envir = getNamespace("revdepcheck"))) {
        stop(sprintf("Environment variable %s is not supported by revdepcheck %s, because it has no revdepcheck:::revdep_check_args(). Please reinstall with remotes::install_github(\"https://github.com/HenrikBengtsson/revdepcheck/tree/feature/check_args\")", sQuote("R_REVDEPCHECK_CHECK_ARGS"), packageVersion("revdepcheck")))
      }
    }

    for (name in c("R_LIBS_USER", "R_LIBS", "R_LIBS_SITE", "R_REVDEPCHECK_TIMEOUT", "R_REVDEPCHECK_NUM_WORKERS", "CRANCACHE_DIR")) {
      message(sprintf("%s=%s", name, sQuote(Sys.getenv(name))))
    }

    check()
    t1 <- Sys.time()

    message(sprintf("Finish time: %s", format(t1)))
    message(sprintf("Total check time: %s", format(t1 - t0)))

    revdepcheck::revdep_report(pkg, all = TRUE)
  }
}

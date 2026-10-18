# Version (development version)

## New Features

 * `check()` now asserts, before doing any other work, that a site
   library pre-populated with the 'callr' package (and its recursive
   dependencies) is on `.Library.site`. Without this, `revdepcheck`'s
   nested 'callr' subprocess calls (e.g. via `crancache`'s binary-cache
   refresh) can fail with `Error in loadNamespace(x) : there is no
   package called 'callr'` well into a check run, because `revdepcheck`
   narrows `.libPaths()` for these subprocesses down to just the
   per-package check library. If not set up, it stops with an
   informative error message explaining how to set `R_LIBS_SITE` in the
   environment *before* starting R (setting it from within an
   already-running R session, e.g. via `revdep/revdepcheck.Renviron`,
   is too late to take effect).

 * Add `callr_site_library()`, `setup_callr_site_library()`, and
   `assert_callr_site_library()`.

 * `check()` now queries and reports on the open file descriptor limit
   (`ulimit -n`) and asserts that it is sufficient for the number of
   parallel workers. If the limit is too low, it stops with an
   informative error message explaining how to increase `ulimit -n`
   or reduce the number of workers.

 * Add `ulimit_n()` and `assert_ulimit_n()`.

# Version 0.8.0 [2026-10-10]

## Significant changes

 * The reverse-dependency file cache location is now unique to the
   time-machine CRAN repository mirror set.

## New Features

 * The default value for R option `revdepcheck.extras.snapshot.source`
   can now be set via environment variable
   `R_REVDEPCHECK_EXTRAS_SNAPSHOT_SOURCE` on package load.

 * Argument `chunk_size` of `revdep_preinstall()` now defaults to
   `parallelly::availableCores()`.

 * Add `crancache_cleanup()`.

## Miscellaneous

 * Now using `futurize()` of the **futurize** package for parallel
   processing, instead of using the **future.apply** package directly.

## Bug Fixes

 * CLI option `--preinstall-children` of `run()` bypassed
   `revdep_children()` memoization and self-dependency filtering.

 * `revdep_preinstall(..., chunk_size = Inf)` crashed and resulted in
   zero packages being installed.

 * `revdep_over_time()` crashed when `dates` was empty, rejected
   numeric or logical values for `none` (e.g. `none = 0` or
   `none = NA`), and failed to cache packages with zero reverse
   dependencies.

 * `revdep_use_tmpdir()` failed to recreate missing target folders in
   `tmpdir` when symlinks were broken, and had documentation typos in
   folder paths.

 * `assert_repos()` produced empty output when R option `repos` was
   unnamed, and formatted unnamed elements as `"=..."` when partially
   named.

 * `revdep_precache()` considered a package cached even if only an older
   version or a non-binary source tarball existed in the crancache, and
   did not correctly identify packages that failed to install when an
   older version was in cache.

 * `revdep_grandchildren()` returned `NULL` instead of an empty
   character vector and failed to memoize when there were no
   grandchildren.

 * `revdep_required_packages()` lacked a default for argument `package`
   and failed to resolve `"."` to the current package.

 * CLI option `--show-check` of `run()` crashed if `00check.log` did not
   exist.

 * `parse_pkgs()` failed to parse whitespace-separated package names.

 * The file cache location for `cran_revdep_on_date()` was not unique
   to the time-machine CRAN repository mirror set.

 * CLI options `--add-updated` and `--add-new` of `run()` did not
   display the updated todo list.

 * `revdep_todo_reset()` dropped column `status` from database table
   `todo`, causing subsequent **revdepcheck** operations to fail.

 * `todo()` added indentation to lines.

 * `assert_repos(verbose = TRUE)` formatted all repositories on a
   single line.

 * Several CLI options of `run()` ignored argument `pkg` when querying
   or displaying the todo list.

 * `revdep_pkgs_with_status()` returned `NULL` instead of an empty
   character vector when no packages matched the status.

 * `parse_pkgs()` could return empty package names.

 * `check()`, `revdep_reset()`, `revdep_todo_reset()`, and
   `revdep_use_tmpdir()` ignored argument `pkg`.

 * `revdep_preinstall()` did not undo R option `Ncpus` and failed to
   report progress after each chunk of packages.

 * `is_symlink()` failed to identify broken symbolic links.

 * CLI option `--add-failure` of `run()` failed to specify the target
   package when adding packages, and did not display the updated todo
   list.

 * `revdep_grandchildren()` memoization ignored argument
   `exclude_children`.

 * `revdep_readme_packages()` reported `NA` instead of `0.0.0.0` for
   packages that were not available.

 * `required_packages()` dropped packages when dependencies spanned
   multiple lines, or when version specifications lacked a leading space.
   It also returned `NULL` instead of an empty character vector when
   no packages were requested.

 * `is_in_crancache()` gave false negatives when multiple versions
   were cached, or because versions were compared as strings instead
   package versions.

 * CLI option `--add-all` of `run()` failed to add reverse
   dependencies.

# Version 0.7.0 [2025-07-10]

## Significant changes
 
 * Now `revdep_preinstall()` defaults to installing 16 packages at the
   time. It used to be one package at the time.

 * Now `revdep_preinstall()` forces installation from source,
   regardless of cached binaries already exist or not.

## New Features

 * `run()` gained argument `force` to control whether already
   installed packages should be re-installed.
 
 * Added argument `chunk_size` to `revdep_preinstall()`, which
   controls how many packages are installed at the same time, which in
   turn affects how frequently **crancache** package library cache is
   updated.

 * Now using [evercran](https://github.com/r-hub/evercran) as the main
   CRAN snapshot database.

 * `run()` sets R option `useFancyQuotes` to FALSE, while running.
 

# Version 0.6.0 [2023-11-07]

## New Features

 * Made replicated calls to `revdep_over_time()` faster by improving
   the caching strategy.

 * `--list-failure`, `--list-error`, `--list-updated`, and
   `--list-new` now reports on the number of packages too.
 
## Bug Fixes

 * `revdep_over_time()` stopped working for CRAN snaphots prior to
   2018-01-01, because the MRAN timemachine was discontinued at the
   end of June 2023.

 * `revdep_over_time()` did not work beyond 2023-03-06, which was when
   the MRAN timemachine stopped gathering new data.  Now we're using
   the RStudio Package Manager (RSPM) instead.


# Version 0.5.0 [2023-01-30]

## New Features

 * Environments variables to be used during checks can be set in an
   Renviron file named `revdep/revdepcheck.Renviron`.  This file is
   loaded by `revdepcheck::check()`.


# Version 0.4.0 [2022-11-21]

## New Features

 * Added `revdep_reset()` which empties and removes
   `revdep/{cache,checks,library}` folders.

 * Now the default number of revdep workers is
   `parallelly::availableCores()`.


# Version 0.3.0 [2021-08-12]

## Significant Changes

 * Now checking also vignettes. This requires using
   HenrikBengtsson/revdepcheck which is a fork of the upstream
   r-lib/revdepcheck. See also feature request
   <https://github.com/r-lib/revdepcheck/issues/277>.


## New Features

 * Now `run()` reports on `R_LIBS`, `R_LIBS_SITE`, `R_LIBS_USER`,
   `R_REVDEPCHECK_NUM_WORKERS` and `CRANCACHE_DIR` in the 'SETUP'
   section.

 * Add `revdep_use_tmpdir()` with corresponding CLI option
   `--use-tmpdir` to host `revdep/{cache,checks,library}` under
   `TMPDIR`, which can be significantly faster on some systems where
   the global file system is slow.

 * `check()` now sets environment variables `R_USER_CACHE_DIR` and
   `XDG_CACHE_HOME` to the absolute path of the `revdep/cache` folder,
   if it exists.  This will simplify checking with a clean cache
   folder.


# Version 0.2.0 [2021-06-07]

## New Features

 * `run()` gained argument `pkg`.

 * Added progress updated to `revdep_precache()` and
   `revdep_preinstall()`.


## Bug Fixes

 * `revdep_preinstall()` would install a lot of packages because it
   installed with `dependencies =` `c("Depends", "Imports",
   "LinkingTo", "Suggests")` instead of `dependencies = TRUE.`  The
   former would install all packages under `Suggests` recursively.

 * `revdep_init()` could give an error under certain circumstances.


# Version 0.1.0 [2021-06-04]

## Siginficant Changes

 * First "official" release.


# Version 0.0.1-9000 [2019-06-13]


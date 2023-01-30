# Version (development version)

 * ...


# Version 0.5.0 [2023-01-30]

### New Features

 * Environments variables to be used during checks can be set in an
   Renviron file named `revdep/revdepcheck.Renviron`.  This file is
   loaded by `revdepcheck::check()`.


# Version 0.4.0 [2022-11-21]

### New Features

 * Added `revdep_reset()` which empties and removes
   `revdep/{cache,checks,library}` folders.

 * Now the default number of revdep workers is
   `parallelly::availableCores()`.


# Version 0.3.0 [2021-08-12]

### Significant Changes

 * Now checking also vignettes. This requires using
   HenrikBengtsson/revdepcheck which is a fork of the upstream
   r-lib/revdepcheck. See also feature request
   <https://github.com/r-lib/revdepcheck/issues/277>.


### New Features

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

### New Features

 * `run()` gained argument `pkg`.

 * Added progress updated to `revdep_precache()` and
   `revdep_preinstall()`.


### Bug Fixes

 * `revdep_preinstall()` would install a lot of packages because it
   installed with `dependencies =` `c("Depends", "Imports",
   "LinkingTo", "Suggests")` instead of `dependencies = TRUE.`  The
   former would install all packages under `Suggests` recursively.

 * `revdep_init()` could give an error under certain circumstances.


# Version 0.1.0 [2021-06-04]

### Siginficant Changes

 * First "official" release.


# Version 0.0.1-9000 [2019-06-13]


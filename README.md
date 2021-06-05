# revdepcheck.extras - Reverse-Dependency Checks from the Command Line (CLI)

This package provides additional functions to be used with the [revdepcheck] package.  It also provide a command-line interface (CLI) for running reverse-dependency package checks directly from the command line.


## Examples

### From R

Check _all_ reverse dependencies running eight checks in parallel:

```r
> options(revdepcheck.num_workers = 8)
> revdepcheck.extras::revdep_check()
```


Check a _specific set_ of reverse-dependency packages:

```r
> library(revdepcheck.extras)
> revdep_init()
> revdep_add(packages = c("pkgA", "pkgB", "pkgQ"))
> revdep_add(packages = revdep_children())
> revdep_add(packages = c(revdep_children(), revdep_grandchildren()))
> revdep_todo()
```


### From the shell

```sh
$ export R_REVDEPCHECK_NUM_WORKERS=8
$ Rscript -e revdepcheck.extras::run
$ Rscript -e revdepcheck.extras::run --help
$ Rscript -e revdepcheck.extras::run --init
$ Rscript -e revdepcheck.extras::run --reset
$ Rscript -e revdepcheck.extras::run --add pkgA pkgB pkgQ
$ Rscript -e revdepcheck.extras::run --add pkgA,pkgB,pkgQ
$ Rscript -e revdepcheck.extras::run --add-children
$ Rscript -e revdepcheck.extras::run --add-grandchildren
$ Rscript -e revdepcheck.extras::run --todo
$ Rscript -e revdepcheck.extras::run --preinstall-children
```


## Installation

To install this package, call:

```sh
remotes::install_github("HenrikBengtsson/revdepcheck.extras")
```


## Disclaimer

I have no intention of submitting this to CRAN or providing end-user support
for it.  I use it for revdepcheck:ing my own packages on a HPC system where
I for instance need to pre-populate the [crancache] cache before running
revdep checks as a job on compute nodes without internet access.


[crancache]: https://github.com/r-lib/crancache
[revdepcheck]: https://github.com/r-lib/revdepcheck

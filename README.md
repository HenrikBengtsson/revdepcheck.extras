# revdepcheck.extras - Reverse-Dependency Checks from the Command Line (CLI)

This package provides additional functions to be used with the [revdepcheck] package.  It also provide a command-line interface (CLI) for running reverse-dependency package checks directly from the command line.


## Example

```sh
$ Rscript -e revdepcheck.extras::run
$ Rscript -e revdepcheck.extras::run --reset
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
I for instance need to pre-populate the [crancheck] cache before running
revdep checks as a job on compute nodes without internet access.


[crancache]: https://github.com/r-lib/crancache
[revdepcheck]: https://github.com/r-lib/revdepcheck

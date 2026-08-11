## Release summary

Previous CRAN version: 1.3.10

Candidate version: 1.4.0

This is the first CRAN update since 2021. It reconciles the package's phonetic
encoders against their published specifications, hardens the native C++ layer,
modernizes the test suite, and refreshes package documentation.

## Test environments

The frozen source tarball was checked on GitHub Actions with R release and
R-devel on Ubuntu. Repository checks also passed on Ubuntu with R release,
R-devel, and R oldrel, on Windows with R release, and on macOS with R release.

The previous CRAN release, 1.3.10, currently reports OK on every platform in
the CRAN check matrix.

## R CMD check results

The exact `phonics_1.4.0.tar.gz` candidate completed `R CMD check --as-cran`
under R release and R-devel with PDF and HTML manual generation enabled.

0 errors | 0 warnings | 0 notes

## Reverse dependencies

The current CRAN reverse-dependency graph contains `epidm` and `joinery` as
reverse imports and `starling` as a reverse suggest.

All three were compared from source with `revdepcheck` against CRAN phonics
1.3.10. There were 0 new problems and 0 packages that failed to check. `epidm`
and `joinery` completed with 0 errors, 0 warnings, and 0 notes. `starling` had
the same pre-existing vignette error with both phonics versions.

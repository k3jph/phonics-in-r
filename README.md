# Phonetic Spelling Algorithms in R

[![Build Status](https://api.travis-ci.org/k3jph/phonics-in-r.svg?branch=master)](https://travis-ci.org/k3jph/phonics-in-r)
[![Coverage Status](https://coveralls.io/repos/github/k3jph/phonics-in-r/badge.svg?branch=develop)](https://coveralls.io/github/k3jph/phonics-in-r?branch=develop)
[![codecov](https://codecov.io/gh/k3jph/phonics-in-r/branch/master/graph/badge.svg)](https://codecov.io/gh/k3jph/phonics-in-r)
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/phonics)](https://cran.r-project.org/package=phonics)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.1041982-blue.svg)](https://dx.doi.org/10.5281/zenodo.1041982)
[![JOSS Status](http://joss.theoj.org/papers/13e41c9bd376fe2fc948f8af10b138b6/status.svg)](http://joss.theoj.org/papers/13e41c9bd376fe2fc948f8af10b138b6)

This is the R package to support phonetic spelling algorithms in R.
Several packages provide the Soundex algorithm.  However, other
algorithms have been developed since Soundex that can also provide
phonetic spelling and test phonetic similarity.

## Algorithms included

* Caverphone
  * Original Caverphone
  * Caverphone 2
* Cologne (Kölner)
* Lein
* Match Rating Approach
  * Encoder
  * Comparison
* Metaphone
* New York State Identification and Intelligence System
  * NYSIIS
  * Modified NYSIIS
* Oxford Name Compression Algorithm
* Phonex
* Roger Root
* Soundex
  * Original Soundex
  * Apache Refined Soundex
* Statistics Canada
  * Census Modified

## Dependencies

* testthat
* roxygen2
* Rcpp
* BH
* data.table

## Contribution guidelines

* Use [GitFlow](http://nvie.com/posts/a-successful-git-branching-model/)
* Write unit tests using [testthat](https://github.com/hadley/testthat)
* Document functions using [roxygen2](https://github.com/yihui/roxygen2)

## For more information

* [Phonics in R website](https://jameshoward.us/phonics-in-r/)
* James P. Howard, II <<jh@jameshoward.us>>

## Acknowledgements

This work used the Extreme Science and Engineering Discovery Environment
(XSEDE), which is supported by National Science Foundation grant number
ACI-1548562. In particular, it used the Comet system at the San Diego
Supercomputing Center (SDSC) through allocations TG-DBS170012 and
TG-ASC150024.

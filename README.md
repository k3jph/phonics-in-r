# Phonetic Spelling Algorithms in R

[![Build Status](https://travis-ci.org/howardjp/phonics.svg?branch=master,osx)](https://travis-ci.org/howardjp/phonics)
[![Coverage Status](https://coveralls.io/repos/howardjp/phonics/badge.svg?branch=master&service=github)](https://coveralls.io/github/howardjp/phonics?branch=master)
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/phonics)](https://CRAN.R-project.org/package=phonics)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1041982.svg)](https://doi.org/10.5281/zenodo.1041982)
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

## Contribution guidelines

* Use [GitFlow](http://nvie.com/posts/a-successful-git-branching-model/)
* Write unit tests using [testthat](https://github.com/hadley/testthat)
* Document functions using [roxygen2](https://github.com/yihui/roxygen2)
* Use [ZenHub](https://www.zenhub.com/) for project management on GitHub

## For more information

* [Phonics in R website](https://jameshoward.us/software/phonics)
* James P. Howard, II &lt;jh@jameshoward.us&gt;

_This work used the Extreme Science and Engineering Discovery Environment
(XSEDE), which is supported by National Science Foundation grant number
ACI-1548562. In particular, it used the Comet system at the San Diego
Supercomputing Center (SDSC) through allocations TG-DBS170012 and
TG-ASC150024._

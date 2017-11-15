# Phonetic Spelling Algorithms in R

[![Build Status](https://travis-ci.org/howardjp/phonics.svg?branch=master,osx)](https://travis-ci.org/howardjp/phonics)
[![Coverage Status](https://coveralls.io/repos/howardjp/phonics/badge.svg?branch=master&service=github)](https://coveralls.io/github/howardjp/phonics?branch=master)
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/phonics)](https://CRAN.R-project.org/package=phonics)
 
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

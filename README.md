# Phonetic Spelling Algorithms in R

![CRAN/METACRAN](https://img.shields.io/cran/v/phonics)
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/phonics)](https://cran.r-project.org/package=phonics)
[![Build (main)](https://github.com/k3jph/phonics-in-r/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/k3jph/phonics-in-r/actions/workflows/R-CMD-check.yaml?query=branch%3Amain)
[![Build (develop)](https://github.com/k3jph/phonics-in-r/actions/workflows/R-CMD-check.yaml/badge.svg?branch=develop)](https://github.com/k3jph/phonics-in-r/actions/workflows/R-CMD-check.yaml?query=branch%3Adevelop)
[![codecov](https://codecov.io/gh/k3jph/phonics-in-r/branch/main/graph/badge.svg)](https://codecov.io/gh/k3jph/phonics-in-r)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/08ad4c6ccb8d4bf59ca47b7524af04be)](https://app.codacy.com/gh/k3jph/phonics-in-r)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.1041982-success.svg)](https://dx.doi.org/10.5281/zenodo.1041982)
[![JOSS Status](http://joss.theoj.org/papers/13e41c9bd376fe2fc948f8af10b138b6/status.svg)](https://joss.theoj.org/papers/10.21105/joss.00480)
[![JSS Status](https://img.shields.io/badge/JSS-10.18637%2Fjss.v095.i08-success.svg)](https://dx.doi.org/10.18637/jss.v095.i08)
[![James Howard](https://jameshoward.us/assets/img/identity/jameshoward-badge.svg)](https://jameshoward.us)

`phonics` provides phonetic encoders for English, German, and French
names, plus the two-stage Match Rating Approach comparison. Every encoder
accepts character vectors and preserves their length and order.

## Installation

Install the released package from CRAN:

```r
install.packages("phonics")
```

Load it and encode one or more names:

```r
library(phonics)

soundex(c("Robert", "Rupert"))
caverphone(c("Peter", "Peady"), modified = TRUE)
phonics(c("Catherine", "Kathryn"), c("soundex", "nysiis", "metaphone"))
```

By default, an input containing characters outside an encoder's supported
alphabet produces a warning and an `NA` result. Set `clean = FALSE` to discard
unsupported characters before encoding. `maxCodeLen` is a validated
nonnegative whole-number bound where supported; Cologne uses unbounded output
when `maxCodeLen = NULL`.

## Algorithms included

| Algorithm | Function and variants |
|:--|:--|
| Caverphone | `caverphone()` (Caverphone 1 and Caverphone 2) |
| Cologne Phonetic | `cologne()` |
| Lein | `lein()` |
| Match Rating Approach | `mra_encode()` and `mra_compare()` |
| Metaphone | `metaphone()` |
| NYSIIS | `nysiis()` (original and USDA modified) |
| Oxford Name Compression Algorithm | `onca()` |
| Phonex | `phonex()` |
| Roger Root | `rogerroot()` |
| Soundex | `soundex()` and `refinedSoundex()` |
| Census Modified Statistics Canada | `statcan()` |

ONCA's source describes an unpublished “anglicised” NYSIIS first stage.
`onca()` therefore uses the documented standard NYSIIS rules; its modified
NYSIIS and Refined Soundex combinations are explicit package extensions.

## Runtime dependencies

The package imports `Rcpp` and `data.table`. Test, vignette, and documentation
tools are development dependencies declared in `DESCRIPTION`.

## Contribution guidelines

- Use [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/).
- Write unit tests using [testthat](https://testthat.r-lib.org/).
- Document functions using [roxygen2](https://roxygen2.r-lib.org/).

## For more information

- [Package reference](https://jameshoward.us/phonics-in-r/)
- Howard, J. P., II (2020). “Phonetic Spelling Algorithm Implementations
  for R.” *Journal of Statistical Software*, 95(8), 1–21.
  [doi:10.18637/jss.v095.i08](https://doi.org/10.18637/jss.v095.i08)

## Acknowledgements

This work used the Extreme Science and Engineering Discovery Environment
(XSEDE), which is supported by National Science Foundation grant number
ACI-1548562. In particular, it used the Comet system at the San Diego
Supercomputing Center (SDSC) through allocations TG-DBS170012 and
TG-ASC150024.

# phonics 1.4.0

This is the first CRAN release of `phonics` since 1.3.10.

## Correctness

- Reconciled every exported phonetic encoder with its primary or authoritative
  algorithm specification and added independent conformance cases for the
  documented rules.
- Corrected user-visible output in Caverphone 2, Cologne, Match Rating
  comparison, original and USDA Modified NYSIIS, Phonex, Roger Root, Refined
  Soundex edge handling, and Census Modified Statistics Canada encoding.
  Examples include the Caverphone 2 initial-`Y` rule, Modified NYSIIS terminal
  and contextual rules, Roger Root's encoding of `Breschnew` (`09462`), and
  Statistics Canada normalization of `garçon` when cleaning is disabled
  (`GRCN`).
- Made all four ONCA variants explicit compositions of NYSIIS followed by
  Soundex, including the package's modified and refined combinations.

## Reliability and portability

- Hardened the native C++ implementations against out-of-bounds lookaround,
  unsafe character indexing, and missing values, and removed the undefined
  `(char)NULL` sentinel behavior.
- Repaired compilation and execution on Alpine Linux with musl libc and added
  regression protection for Metaphone `GH` boundaries.
- Removed the unnecessary Boost usage and the `BH` package dependency.

## API and validation

- Defined `maxCodeLen` as a single non-negative whole number wherever it is
  supported. Invalid, missing, fractional, non-finite, negative, and
  non-scalar values now fail with a package-owned diagnostic before reaching
  native or substring operations.
- Cologne retains `maxCodeLen = NULL` as its historical unbounded default;
  explicit values now validate and truncate the result.
- The USDA Modified NYSIIS variant now warns and returns `NA` for names ending
  in `JR` or `SR`, as required by its published input rules.
- Corrected the advertised modified-ONCA dispatch path and all-`NA` Match
  Rating comparison behavior.

## Testing

- Migrated to testthat edition 3 and a conventional CRAN-executed test entry
  point.
- Added algorithm-conformance, public-contract, integration, boundary,
  historical-regression, and broad fixture-based coverage across all exported
  functions and algorithm variants.

## Documentation

- Completed a package-wide reference audit, committed reproducible roxygen
  output, and added an independent documentation-freshness check.
- Corrected citations, algorithm authorities, argument contracts, examples,
  and the package reference material.

## Website

- The canonical project website is now <https://phonics.jameshoward.us>.


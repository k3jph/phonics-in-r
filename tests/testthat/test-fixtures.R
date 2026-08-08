fixture_provenance <- c(
    "caverphone-modified.csv" = "external reference",
    "caverphone.csv" = "external reference",
    "cologne.csv" = "external reference",
    "lein.csv" = "external cross-check",
    "metaphone.csv" = "legacy regression; original source unrecorded",
    "mra-compare.csv" = "external reference",
    "mra-encode.csv" = "external reference",
    "nysiis-modified.csv" = "external reference",
    "nysiis.csv" = "external reference plus historical regressions",
    "onca-modified-refined.csv" = "local integration regression",
    "onca-modified.csv" = "local integration regression",
    "onca-refined.csv" = "local integration regression",
    "onca.csv" = "external reference",
    "phonex.csv" = "external reference",
    "phonics.csv" = "local end-to-end regression",
    "rogerroot.csv" = "published reference plus local caveat",
    "soundex-refined.csv" = "external reference",
    "soundex.csv" = "external reference",
    "statcan.csv" = "published reference"
)

test_that("every fixture has provenance and a well-formed unique corpus", {
    files <- sort(list.files(test_path(), pattern = "[.]csv$"))

    expect_setequal(files, names(fixture_provenance))
    expect_false(anyDuplicated(names(fixture_provenance)) > 0L)

    for (file in files) {
        fixture <- read.csv(
            test_path(file),
            comment.char = "#",
            stringsAsFactors = FALSE,
            colClasses = "character",
            encoding = "UTF-8",
            check.names = FALSE
        )

        expect_gte(nrow(fixture), 1L, info = file)
        expect_gte(ncol(fixture), 2L, info = file)
        expect_false(anyDuplicated(fixture) > 0L, info = file)
        expect_true(all(vapply(fixture, is.character, logical(1))), info = file)
        expect_true(nzchar(fixture_provenance[[file]]), info = file)
    }
})

test_that("NYSIIS reference corpora are preserved", {
    expect_encoder_fixture(encoder_specs$nysiis)
    expect_encoder_fixture(encoder_specs$nysiis_modified)
})

test_that("NYSIIS retains the historical H-rule correction", {
    expect_identical(
        nysiis(c("CHRISTINA", "CHERYL", "NEHEMIAH", "NOAH")),
        c("CRASTA", "CARYL", "NAHAN", "N")
    )
})

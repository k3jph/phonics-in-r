test_that("Caverphone reference corpora are preserved", {
    expect_encoder_fixture(encoder_specs$caverphone)
    expect_encoder_fixture(encoder_specs$caverphone_modified)
})

test_that("Caverphone variants remain distinct", {
    input <- c("Stevenson", "Peter", "Lee")

    expect_false(identical(
        caverphone(input),
        caverphone(input, modified = TRUE)
    ))
})

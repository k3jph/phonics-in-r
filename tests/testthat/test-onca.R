test_that("Every ONCA variant has direct corpus coverage", {
    expect_encoder_fixture(encoder_specs$onca)
    expect_encoder_fixture(encoder_specs$onca_modified)
    expect_encoder_fixture(encoder_specs$onca_refined)
    expect_encoder_fixture(encoder_specs$onca_modified_refined)
})

test_that("ONCA variants dispatch to their intended components", {
    input <- c("Andersen", "Peter", "Stevenson")

    expect_identical(
        onca(input),
        soundex(nysiis(input, maxCodeLen = 8), maxCodeLen = 4)
    )
    expect_identical(
        onca(input, modified = TRUE),
        soundex(nysiis(input, maxCodeLen = 8, modified = TRUE), maxCodeLen = 4)
    )
    expect_identical(
        onca(input, refined = TRUE),
        refinedSoundex(nysiis(input, maxCodeLen = 8), maxCodeLen = 4)
    )
    expect_identical(
        onca(input, modified = TRUE, refined = TRUE),
        refinedSoundex(
            nysiis(input, maxCodeLen = 8, modified = TRUE),
            maxCodeLen = 4
        )
    )
})

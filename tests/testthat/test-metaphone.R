test_that("Metaphone legacy regression corpus is preserved", {
    expect_encoder_fixture(encoder_specs$metaphone)
})

test_that("Metaphone handles GH and end-of-word boundaries safely", {
    expect_identical(
        metaphone(c("sigh", "rough", "bughouse", "funhouse")),
        c("SF", "RF", "BFS", "FNHS")
    )

    expect_identical(
        metaphone(c("", LETTERS, NA_character_)),
        c("", LETTERS, NA_character_)
    )
})

test_that("Metaphone never exceeds maxCodeLen when a rule emits two symbols", {
    expect_identical(metaphone("AX", maxCodeLen = 2), "AK")
})

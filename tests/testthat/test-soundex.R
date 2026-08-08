test_that("Soundex reference corpora are preserved", {
    expect_encoder_fixture(encoder_specs$soundex)
    expect_encoder_fixture(encoder_specs$soundex_refined)
})

test_that("Soundex retains single-character padding", {
    expect_identical(soundex(LETTERS), paste0(LETTERS, "000"))
    expect_identical(soundex(c("D", "DA")), c("D000", "D000"))
})

test_that("Soundex implementations handle empty and one-letter inputs", {
    expect_identical(
        soundex(c("", LETTERS, NA_character_)),
        c("", paste0(LETTERS, "000"), NA_character_)
    )
    expect_identical(
        refinedSoundex(c("", LETTERS, NA_character_)),
        c("", LETTERS, NA_character_)
    )
})

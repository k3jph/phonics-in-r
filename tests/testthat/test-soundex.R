context("soundex")

test_that("The soundex algorithm implementation accurately maps strings to soundexes", {
    skip_on_cran()

    test_data <- read.csv("soundex.csv", comment.char = "#", stringsAsFactors = FALSE)
    expect_true(all(soundex(test_data$word) == test_data$value))
    test_data$test_soundexes <- soundex(test_data$word)
    expect_true(all(test_data$test_soundexes == test_data$value))
})

test_that("The soundex algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- soundex(NA_character_)
    expect_true(is.na(test_data))
})

test_that("The refined soundex algorithm implementation accurately maps strings to soundexes", {
    skip_on_cran()

    test_data <- read.csv("soundex-refined.csv", comment.char = "#", stringsAsFactors = FALSE)
    expect_true(all(refinedSoundex(test_data$word) == test_data$value))
    test_data$test_soundexes <- refinedSoundex(test_data$word)
    expect_true(all(test_data$test_soundexes == test_data$value))
})

test_that("The refined soundex algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- refinedSoundex(NA_character_)
    expect_true(is.na(test_data))
})

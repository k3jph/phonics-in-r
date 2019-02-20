context("soundex")

##  Test the soundex algorithm
test_that("Test that soundex works", {
    skip_on_cran()

    test <- read.csv("soundex.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- soundex(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(soundex(test$word[i]) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_false(is.na(testValue <- soundex(test$word[i], clean = FALSE)))
        } else
            expect_true(soundex(test$word[i], clean = FALSE) == test$value[i])
    }

})

test_that("The soundex algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- soundex(NA_character_)
    expect_true(is.na(test_data))
})

test_that("The soundex algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- soundex(NULL)
    expect_true(is.na(test_data))
})

##  Test the refined soundex algorithm
test_that("Test that refined soundex works", {
    skip_on_cran()

    test <- read.csv("soundex-refined.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- refinedSoundex(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(refinedSoundex(test$word[i]) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_false(is.na(testValue <- refinedSoundex(test$word[i], clean = FALSE)))
        } else
            expect_true(refinedSoundex(test$word[i], clean = FALSE) == test$value[i])
    }

})

test_that("The refined soundex algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- refinedSoundex(NA_character_)
    expect_true(is.na(test_data))
})

test_that("The refined soundex algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- refinedSoundex(NULL)
    expect_true(is.na(test_data))
})

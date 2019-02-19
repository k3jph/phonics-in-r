context("nysiis")

##  Test the Caverphone algorithm
test_that("Test that NYSIIS works", {
    skip_on_cran()

    test <- read.csv("nysiis.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- nysiis(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(nysiis(test$word[i]) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(nysiis(test$word[i], clean = FALSE)))
        else
            expect_true(nysiis(test$word[i], clean = FALSE) == test$value[i])
    }

})

test_that("The NYSIIS algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- nysiis(NA_character_)
    expect_true(is.na(test_data))
})

test_that("The NYSIIS algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- nysiis(NULL)
    expect_true(is.na(test_data))
})

##  Test the Modified NYSIIS algorithm
test_that("Test that modified NYSIIS works", {
    skip_on_cran()

    test <- read.csv("nysiis-modified.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- nysiis(test$word[i], modified = TRUE))
            expect_true(is.na(testValue))
        } else
            expect_true(nysiis(test$word[i], modified = TRUE) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(nysiis(test$word[i], modified = TRUE, clean = FALSE)))
        else
            expect_true(nysiis(test$word[i], modified = TRUE, clean = FALSE) == test$value[i])
    }

})

test_that("The modified NYSIIS algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- nysiis(NA_character_, modified = TRUE)
    expect_true(is.na(test_data))
})

test_that("The modified NYSIIS algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- nysiis(NULL, modified = TRUE)
    expect_true(is.na(test_data))
})

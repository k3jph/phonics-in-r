context("nysiis")

##  Test the Caverphone algorithm
test_that("Test that NYSIIS works", {
    skip_on_cran()

    test <- read.csv("nysiis.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where ignoreNonAlpha is FALSE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- nysiis(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(nysiis(test$word[i]) == test$value[i])
    }

    ## Test for cases where ignoreNonAlpha is TRUE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- nysiis(test$word[i], ignoreNonAlpha = TRUE))
            expect_false(is.na(testValue))
        } else
            expect_true(nysiis(test$word[i], ignoreNonAlpha = TRUE) == test$value[i])
    }

})

##  Test the Modified NYSIIS algorithm
test_that("Test that modified NYSIIS works", {
    skip_on_cran()

    test <- read.csv("nysiis-modified.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where ignoreNonAlpha is FALSE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- nysiis(test$word[i], modified = TRUE))
            expect_true(is.na(testValue))
        } else
            expect_true(nysiis(test$word[i], modified = TRUE) == test$value[i])
    }

    ## Test for cases where ignoreNonAlpha is TRUE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- nysiis(test$word[i], modified = TRUE, ignoreNonAlpha = TRUE))
            expect_false(is.na(testValue))
        } else
            expect_true(nysiis(test$word[i], modified = TRUE, ignoreNonAlpha = TRUE) == test$value[i])
    }

})

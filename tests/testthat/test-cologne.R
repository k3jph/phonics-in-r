context("cologne")

##  Test the Cologne algorithm
test_that("Test that cologne works", {
    skip_on_cran()

    test <- read.csv("cologne.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- cologne(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(cologne(test$word[i]) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(cologne(test$word[i], clean = FALSE)))
        else
            expect_true(cologne(test$word[i], clean = FALSE) == test$value[i])
    }

})

test_that("The Cologne algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- cologne(NA_character_)
    expect_true(is.na(test_data))
})

test_that("The Cologne algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- cologne(NULL)
    expect_true(is.na(test_data))
})

context("rogerroot")

##  Test the RogerRoot algorithm
test_that("Test that RogerRoot works", {
    skip_on_cran()

    test <- read.csv("rogerroot.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where ignoreNonAlpha is FALSE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- rogerroot(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(rogerroot(test$word[i]) == test$value[i])
    }

    ## Test for cases where ignoreNonAlpha is TRUE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- rogerroot(test$word[i], ignoreNonAlpha = TRUE))
            expect_false(is.na(testValue))
        } else
            expect_true(rogerroot(test$word[i], ignoreNonAlpha = TRUE) == test$value[i])
    }

})

context("mra")

##  This is structured a bit differently from the other tests in order
##  to test both the encoder and the comparison.

##  Test the MRA encoding algorithm
test_that("Test that MRA encoder works", {
    skip_on_cran()

    test <- read.csv("mra-encode.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- mra_encode(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(mra_encode(test$word[i]) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(mra_encode(test$word[i], clean = FALSE)))
        else
            expect_true(mra_encode(test$word[i], clean = FALSE) == test$value[i])
    }

})

test_that("The MRA encoder algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- mra_encode(NA_character_)
    expect_true(is.na(test_data))
})

test_that("The MRA encoder algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- mra_encode(NULL)
    expect_true(is.na(test_data))
})

##  Test the MRA compare algorithm
test_that("Test that MRA comparison works", {
    skip_on_cran()

    test <- read.csv("mra-compare.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = c("character", "character"), encoding = "UTF-8")
    for(i in 1:nrow(test))
        expect_true(mra_compare(mra_encode(test$word1[i]), mra_encode(test$word2[i])) == test$value[i])
    test$test <- mra_compare(mra_encode(test$word1), mra_encode(test$word2))
    for(i in 1:nrow(test))
        expect_true(test$test[i] == test$value[i])
})

context("caverphone")

##  Test the Caverphone algorithm
test_that("Test that Caverphone works", {
    skip_on_cran()

    test <- read.csv("caverphone.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- caverphone(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(caverphone(test$word[i]) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(caverphone(test$word[i], clean = FALSE)))
        else
            expect_true(caverphone(test$word[i], clean = FALSE) == test$value[i])
    }

})

test_that("The Caverphone algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- caverphone(NA_character_)
    expect_true(is.na(test_data))
})

test_that("The Caverphone algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- caverphone(NULL)
    expect_true(is.na(test_data))
})

##  Test the Modified Caverphone algorithm
test_that("Test that Modified Caverphone works", {
    skip_on_cran()

    test <- read.csv("caverphone-modified.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- caverphone(test$word[i], modified = TRUE))
            expect_true(is.na(testValue))
        } else
            expect_true(caverphone(test$word[i], modified = TRUE) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(caverphone(test$word[i], clean = FALSE, modified = TRUE)))
        else
            expect_true(caverphone(test$word[i], clean = FALSE, modified = TRUE) == test$value[i])
    }

})

test_that("The Modified Caverphone algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- caverphone(NA_character_, modified = TRUE)
    expect_true(is.na(test_data))
})

test_that("The Modified Caverphone algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- caverphone(NULL, modified = TRUE)
    expect_true(is.na(test_data))
})

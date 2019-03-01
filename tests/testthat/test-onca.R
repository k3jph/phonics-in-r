context("onca")

##  Test the ONCA algorithm
test_that("Test that ONCA works", {
    skip_on_cran()

    test <- read.csv("onca.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- onca(test$word[i]))
            expect_true(is.na(testValue))
        } else
            expect_true(onca(test$word[i]) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(onca(test$word[i], clean = FALSE)))
        else
            expect_true(onca(test$word[i], clean = FALSE) == test$value[i])
    }

})

test_that("The ONCA algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- onca(NA_character_)
    expect_true(is.na(test_data))
})

test_that("The ONCA algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- onca(NULL)
    expect_true(is.na(test_data))
})

##  Test the ONCA (modified) algorithm
test_that("Test that ONCA (modified) works", {
    skip_on_cran()

    test <- read.csv("onca-modified.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- onca(test$word[i], modified = TRUE))
            expect_true(is.na(testValue))
        } else
            expect_true(onca(test$word[i], modified = TRUE) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(onca(test$word[i], clean = FALSE, modified = TRUE)))
        else
            expect_true(onca(test$word[i], clean = FALSE, modified = TRUE) == test$value[i])
    }

})

test_that("The ONCA (modified) algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- onca(NA_character_, modified = TRUE)
    expect_true(is.na(test_data))
})

test_that("The ONCA (modified) algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- onca(NULL, modified = TRUE)
    expect_true(is.na(test_data))
})

##  Test the ONCA (refined) algorithm
test_that("Test that ONCA (refined) works", {
    skip_on_cran()

    test <- read.csv("onca-refined.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- onca(test$word[i], refined = TRUE))
            expect_true(is.na(testValue))
        } else
            expect_true(onca(test$word[i], refined = TRUE) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(onca(test$word[i], clean = FALSE, refined = TRUE)))
        else
            expect_true(onca(test$word[i], clean = FALSE, refined = TRUE) == test$value[i])
    }

})

test_that("The ONCA (refined) algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- onca(NA_character_, refined = TRUE)
    expect_true(is.na(test_data))
})

test_that("The ONCA (refined) algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- onca(NULL, refined = TRUE)
    expect_true(is.na(test_data))
})

##  Test the ONCA (modified, refined) algorithm
test_that("Test that ONCA (modified, refined) works", {
    skip_on_cran()

    test <- read.csv("onca-modified-refined.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i])) {
            expect_warning(testValue <- onca(test$word[i], modified = TRUE, refined = TRUE))
            expect_true(is.na(testValue))
        } else
            expect_true(onca(test$word[i], modified = TRUE, refined = TRUE) == test$value[i])
    }

    ## Test for cases where clean = FALSE, which should not
    ## return NA, so we are going to assume that's an error
    for(i in 1:nrow(test)) {
        if(is.na(test$value[i]))
            expect_false(is.na(onca(test$word[i], clean = FALSE, modified = TRUE, refined = TRUE)))
        else
            expect_true(onca(test$word[i], clean = FALSE, modified = TRUE, refined = TRUE) == test$value[i])
    }

})

test_that("The ONCA (modified, refined) algorithm implementation can handle NAs", {
    skip_on_cran()

    test_data <- onca(NA_character_, modified = TRUE, refined = TRUE)
    expect_true(is.na(test_data))
})

test_that("The ONCA (modified, refined) algorithm implementation can handle NULLs", {
    skip_on_cran()

    test_data <- onca(NULL, modified = TRUE, refined = TRUE)
    expect_true(is.na(test_data))
})

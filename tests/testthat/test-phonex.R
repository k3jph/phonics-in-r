context("phonex")

##  Test the Phonex algorithm
test_that("Phonex works", {
    skip_on_cran()

    test <- read.csv("phonex.csv", comment.char="#", stringsAsFactors = FALSE, colClasses = c("character", "character"), encoding = "UTF-8")
    for(i in 1:nrow(test))
        expect_true(phonex(test$word[i]) == test$value[i])
    test$test <- phonex(test$word)
    for(i in 1:nrow(test))
        expect_true(test$test[i] == test$value[i])
})

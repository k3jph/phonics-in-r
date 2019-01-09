context("statcan")

##  Test the statcan algorithm
test_that("StatCan works", {
    skip_on_cran()

    test <- read.csv("statcan.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = c("character", "character"), encoding = "UTF-8")
    for(i in 1:nrow(test))
        expect_true(statcan(test$word[i]) == test$value[i])
    test$test <- statcan(test$word)
    for(i in 1:nrow(test))
        expect_true(test$test[i] == test$value[i])
})

context("rogerroot")

##  Test the Roger Root name encoding
test_that("RogerRoot works", {
    skip_on_cran()

    test <- read.csv("rogerroot.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = c("character", "character"), encoding = "UTF-8")
    for(i in 1:nrow(test))
        expect_true(rogerroot(test$word[i]) == test$value[i])
    test$test <- rogerroot(test$word)
    for(i in 1:nrow(test))
        expect_true(test$test[i] == test$value[i])
})

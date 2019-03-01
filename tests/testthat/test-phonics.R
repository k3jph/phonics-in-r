context("phonics")

##  Test the phonics driver
test_that("Test that phonics works", {
    skip_on_cran()

    methodList <- c("caverphone", "caverphone.modified", "cologne",
                    "lein", "metaphone", "nysiis", "nysiis.modified",
                    "onca", "onca.modified", "onca.refined",
                    "onca.modified.refined", "phonex", "rogerroot",
                    "soundex", "soundex.refined", "statcan")

    test <- read.csv("phonics.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = rep("character", 2), encoding = "UTF-8")

    ## Test for cases where clean = TRUE, we don't test for NAs since
    ## all the individual algorithms test for NA

    testValue <- phonics(test$word, method = methodList)

    expect_true(all(testValue == test))
})

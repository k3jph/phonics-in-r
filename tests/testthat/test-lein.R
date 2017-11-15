context("lein")

##  Test the lein algorithm
test <- read.csv("lein.csv", comment.char = "#", stringsAsFactors = FALSE, colClasses = c("character", "character"), encoding = "UTF-8")
for(i in 1:nrow(test))
    expect_true(lein(test$word[i]) == test$value[i])
test$test <- lein(test$word)
for(i in 1:nrow(test))
    expect_true(test$test[i] == test$value[i])

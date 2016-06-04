context("metaphone")

test_that("The metaphone algorithm implementation accurately maps strings to metaphones", {
  test_data <- read.csv("metaphone.csv", comment.char = "#", stringsAsFactors = FALSE)
  expect_true(all(metaphone(test_data$word) == test_data$value))
  test_data$test_metaphones <- metaphone(test_data$word)
  expect_true(all(test_data$test_metaphones == test_data$value))
})

test_that("The metaphone algorithm implementation can handle NAs", {
  test_data <- metaphone(NA_character_)
  expect_true(is.na(test_data))
})

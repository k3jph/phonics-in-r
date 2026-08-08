test_that("Phonex reference corpus is preserved", {
    expect_encoder_fixture(encoder_specs$phonex)
})

test_that("Phonex normalizes its documented German characters", {
    expect_no_warning(result <- phonex(c("Äbel", "Übel", "Öbel", "groß")))
    expect_false(anyNA(result))
})

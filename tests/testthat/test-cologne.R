test_that("Cologne reference corpus is preserved", {
    expect_encoder_fixture(encoder_specs$cologne)
})

test_that("Cologne handles its documented German alphabet", {
    expect_no_warning(result <- cologne(c("Müller", "Lüderscheidt", "groß")))
    expect_false(anyNA(result))
    expect_match(result, "^[0-8]*$", all = TRUE)
})

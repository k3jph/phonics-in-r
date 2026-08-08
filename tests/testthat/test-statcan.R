test_that("Statistics Canada reference corpus is preserved", {
    expect_encoder_fixture(encoder_specs$statcan)
})

test_that("Statistics Canada normalizes its supported accented letters", {
    accented <- c("Àbel", "Ébel", "Îbel", "Ôbel", "Übel", "Ÿbel", "Çbel")
    ascii <- c("Abel", "Ebel", "Ibel", "Obel", "Ubel", "Ybel", "Cbel")

    expect_no_warning(actual <- statcan(accented))
    expect_identical(actual, statcan(ascii))
})

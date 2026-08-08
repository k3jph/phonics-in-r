test_that("MRA encoder reference corpus is preserved", {
    expect_encoder_fixture(encoder_specs$mra)
})

test_that("MRA encoder has scalar and vector contracts", {
    input <- c("Byrne", "Boern", "Christopher", "", NA_character_)

    expect_scalar_vector_equivalence(encoder_specs$mra, input)
    expect_identical(mra_encode("Byrne"), "BYRN")
    expect_identical(mra_encode("Christopher"), "CHRPHR")
    expect_identical(mra_encode(""), "")
    expect_identical(mra_encode(NA_character_), NA_character_)
    expect_identical(mra_encode(NULL), NA_character_)
})

test_that("MRA comparison reference corpus is preserved", {
    fixture <- read.csv(
        test_path("mra-compare.csv"),
        comment.char = "#",
        stringsAsFactors = FALSE,
        colClasses = "character",
        encoding = "UTF-8"
    )
    actual <- mra_compare(
        mra_encode(fixture[[1]]),
        mra_encode(fixture[[2]])
    )

    expect_identical(actual, as.logical(fixture[[3]]))
})

test_that("MRA comparison handles vector boundaries and recycling", {
    expect_true(mra_compare("SMTH", "SMTH"))
    expect_false(mra_compare("ABC", "XYZ"))
    expect_true(mra_compare("", ""))
    expect_false(mra_compare("", "ABC"))

    expect_identical(mra_compare(NA_character_, "SMTH"), NA)
    expect_identical(
        mra_compare(c(NA_character_, "SMTH"), c("JNS", "SMTH")),
        c(NA, TRUE)
    )
    expect_identical(
        mra_compare(c(NA_character_, NA_character_), c("ABC", "DEF")),
        c(NA, NA)
    )
    expect_identical(mra_compare(character(), character()), logical())

    recycled <- mra_compare("SMTH", c("SMTH", "JNS"))
    scalar <- c(mra_compare("SMTH", "SMTH"), mra_compare("SMTH", "JNS"))
    expect_identical(recycled, scalar)
    expect_error(mra_compare(c("ABC", "DEF"), c("ABC", "DEF", "GHI")))
})

test_that("MRA comparison consumes encoded values", {
    left <- mra_encode(c("Byrne", "Smith", "Christopher"))
    right <- mra_encode(c("Boern", "Smyth", "Kristoffer"))

    expect_identical(
        mra_compare(left, right),
        vapply(
            seq_along(left),
            function(i) mra_compare(left[i], right[i]),
            logical(1)
        )
    )
})

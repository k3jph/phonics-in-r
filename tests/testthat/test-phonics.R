test_that("phonics preserves the complete integration fixture", {
    fixture <- read.csv(
        test_path("phonics.csv"),
        comment.char = "#",
        stringsAsFactors = FALSE,
        colClasses = "character",
        encoding = "UTF-8",
        check.names = FALSE
    )

    expect_identical(phonics(fixture$word, phonics_methods), fixture)
})

test_that("phonics dispatches one, several, and every method in order", {
    input <- c("Peter", "Peady", NA_character_)

    one <- phonics(input, "soundex")
    expect_s3_class(one, "data.frame")
    expect_identical(names(one), c("word", "soundex"))
    expect_identical(one$soundex, soundex(input))

    selected <- c("statcan", "metaphone", "caverphone.modified")
    several <- phonics(input, selected)
    expect_identical(names(several), c("word", selected))
    expect_identical(several$statcan, statcan(input))
    expect_identical(several$metaphone, metaphone(input))
    expect_identical(
        several$caverphone.modified,
        caverphone(input, modified = TRUE)
    )

    complete <- phonics(input, phonics_methods)
    expect_identical(names(complete), c("word", phonics_methods))
    expect_identical(complete$word, input)
    expect_identical(rownames(complete), as.character(seq_along(input)))
})

test_that("every phonics column equals direct encoder invocation", {
    input <- c("Smith", "Jones", "", NA_character_)
    actual <- phonics(input, phonics_methods)

    for (method in phonics_methods) {
        expect_identical(
            actual[[method]],
            phonics_method_calls[[method]](input, TRUE),
            info = method
        )
    }
})

test_that("phonics characterizes method-list boundaries", {
    input <- c("Smith", "Jones")

    expect_warning(
        unknown <- phonics(input, "not-an-algorithm"),
        "unknown phonetic spelling algorithm"
    )
    expect_identical(unknown, data.frame(word = input))

    expect_identical(
        phonics(input, character()),
        data.frame(word = input)
    )

    duplicated <- phonics(input, c("soundex", "soundex"))
    expect_identical(names(duplicated), c("word", "soundex"))
    expect_identical(duplicated$soundex, soundex(input))
})

test_that("phonics propagates clean and preserves mixed row order", {
    input <- c("Smith", "Smith-Jones", NA_character_, "")

    expect_warning(
        clean <- phonics(input, "soundex", clean = TRUE),
        "unknown characters"
    )
    expect_identical(clean$soundex, suppressWarnings(soundex(input)))

    expect_no_warning(unclean <- phonics(input, "soundex", clean = FALSE))
    expect_identical(unclean$soundex, soundex(input, clean = FALSE))
    expect_identical(unclean$word, input)
})

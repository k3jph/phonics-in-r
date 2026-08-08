test_that("every encoder has scalar/vector equivalence", {
    input <- c(
        "Smith", "SMITH", " smith ", "Smith-Jones", "Smith2", "12345",
        "", "Übel", NA_character_
    )

    for (spec in encoder_specs)
        expect_scalar_vector_equivalence(spec, input)
})

test_that("every encoder handles empty, one-character, NA, and NULL inputs", {
    for (spec in encoder_specs) {
        expect_no_warning(empty <- call_encoder(spec, ""), info = spec$name)
        expect_identical(empty, "", info = spec$name)

        expect_no_warning(letters <- call_encoder(spec, LETTERS), info = spec$name)
        expect_length(letters, length(LETTERS), info = spec$name)
        expect_false(anyNA(letters), info = spec$name)

        expect_identical(
            call_encoder(spec, NA_character_),
            NA_character_,
            info = paste(spec$name, "NA")
        )
        expect_identical(
            call_encoder(spec, NULL),
            NA_character_,
            info = paste(spec$name, "NULL")
        )
    }
})

test_that("clean semantics are explicit across every encoder", {
    for (spec in encoder_specs) {
        expect_identical(
            call_encoder(spec, "smith"),
            call_encoder(spec, "SMITH"),
            info = paste(spec$name, "case normalization")
        )

        expect_warning(
            spaced <- call_encoder(spec, " smith "),
            "unknown characters",
            info = paste(spec$name, "spaces")
        )
        expect_identical(spaced, NA_character_, info = spec$name)

        expect_warning(
            hyphenated <- call_encoder(spec, "Smith-Jones"),
            "unknown characters",
            info = paste(spec$name, "hyphen")
        )
        expect_identical(hyphenated, NA_character_, info = spec$name)

        expect_warning(
            digits <- call_encoder(spec, "Smith2"),
            "unknown characters",
            info = paste(spec$name, "digits")
        )
        expect_identical(digits, NA_character_, info = spec$name)

        expect_no_warning(
            unclean <- call_encoder(spec, "Smith-Jones", clean = FALSE),
            info = paste(spec$name, "clean = FALSE")
        )
        expect_identical(
            unclean,
            call_encoder(spec, "SmithJones"),
            info = paste(spec$name, "clean = FALSE fallback")
        )

        if (spec$accepts_umlaut) {
            expect_no_warning(umlaut <- call_encoder(spec, "Übel"), info = spec$name)
            expect_false(is.na(umlaut), info = spec$name)
        } else {
            expect_warning(
                umlaut <- call_encoder(spec, "Übel"),
                "unknown characters",
                info = spec$name
            )
            expect_identical(umlaut, NA_character_, info = spec$name)
        }
    }
})

test_that("mixed and reasonably large vectors preserve length and order", {
    mixed <- c("Smith", "Smith-Jones", "", NA_character_, "Jones")
    large <- rep(c("Smith", "Jones", "Brown", "", NA_character_), 100L)

    for (spec in encoder_specs) {
        expect_scalar_vector_equivalence(spec, mixed)
        expect_scalar_vector_equivalence(spec, large)
    }
})

test_that("maxCodeLen supports safe boundaries and rejects invalid values", {
    max_specs <- encoder_specs[setdiff(
        names(encoder_specs),
        c("cologne", "mra")
    )]
    invalid <- list(-1, NA_real_, 1.5, "4", c(4, 5), Inf)

    for (spec in max_specs) {
        expect_identical(
            call_encoder(spec, "Stevenson", extra = list(maxCodeLen = 0)),
            "",
            info = paste(spec$name, "maxCodeLen = 0")
        )

        for (value in c(1, 4, 1000)) {
            result <- call_encoder(
                spec,
                "Stevenson",
                extra = list(maxCodeLen = value)
            )
            expect_lte(
                nchar(result),
                value,
                info = paste(spec$name, "maxCodeLen =", value)
            )
        }

        for (value in invalid) {
            expect_error(
                call_encoder(
                    spec,
                    "Stevenson",
                    extra = list(maxCodeLen = value)
                ),
                "maxCodeLen",
                info = spec$name
            )
        }

        if (identical(spec$name, "Caverphone") ||
            identical(spec$name, "Modified Caverphone")) {
            expect_identical(
                call_encoder(spec, "Stevenson", extra = list(maxCodeLen = NULL)),
                call_encoder(spec, "Stevenson"),
                info = paste(spec$name, "explicit NULL default")
            )
        } else {
            expect_error(
                call_encoder(spec, "Stevenson", extra = list(maxCodeLen = NULL)),
                "maxCodeLen",
                info = paste(spec$name, "maxCodeLen = NULL")
            )
        }
    }
})

test_that("Cologne maxCodeLen remains a characterized historical no-op", {
    baseline <- cologne("Müller-Lüdenscheidt")

    expect_identical(cologne("Müller-Lüdenscheidt", maxCodeLen = 0), baseline)
    expect_identical(cologne("Müller-Lüdenscheidt", maxCodeLen = 1), baseline)
    expect_identical(cologne("Müller-Lüdenscheidt", maxCodeLen = 1000), baseline)
    expect_identical(cologne("Müller-Lüdenscheidt", maxCodeLen = -1), baseline)
    expect_identical(cologne("Müller-Lüdenscheidt", maxCodeLen = NA), baseline)
    expect_identical(cologne("Müller-Lüdenscheidt", maxCodeLen = 1.5), baseline)
    expect_identical(cologne("Müller-Lüdenscheidt", maxCodeLen = "4"), baseline)
    expect_identical(cologne("Müller-Lüdenscheidt", maxCodeLen = c(1, 2)), baseline)
})

test_that("bounded deterministic properties hold for ASCII inputs", {
    set.seed(1400)
    generated <- vapply(
        sample(1:14, 250, replace = TRUE),
        function(size) paste(sample(LETTERS, size, replace = TRUE), collapse = ""),
        character(1)
    )

    for (spec in encoder_specs) {
        expect_no_error(upper <- call_encoder(spec, generated), info = spec$name)
        lower <- call_encoder(spec, tolower(generated))

        expect_identical(upper, lower, info = paste(spec$name, "case invariant"))
        expect_identical(
            upper,
            call_encoder(spec, generated),
            info = paste(spec$name, "deterministic")
        )
        expect_length(upper, length(generated), info = spec$name)
        expect_true(all(grepl(spec$alphabet, upper)), info = spec$name)
        expect_true(all(nchar(upper) <= spec$max_code_len), info = spec$name)
    }
})

read_phonics_fixture <- function(file) {
    fixture <- read.csv(
        test_path(file),
        comment.char = "#",
        stringsAsFactors = FALSE,
        colClasses = "character",
        encoding = "UTF-8",
        check.names = FALSE
    )

    names(fixture)[seq_len(min(2L, ncol(fixture)))] <- c("word", "value")
    fixture
}

call_encoder <- function(spec, word, clean = TRUE, extra = list()) {
    do.call(
        spec$encoder,
        c(list(word = word), spec$args, extra, list(clean = clean))
    )
}

expect_encoder_fixture <- function(spec) {
    fixture <- read_phonics_fixture(spec$fixture)
    valid <- !is.na(fixture$value)

    if (any(!valid)) {
        expect_warning(
            actual <- call_encoder(spec, fixture$word),
            "unknown characters",
            info = spec$fixture
        )
    } else {
        expect_no_warning(actual <- call_encoder(spec, fixture$word))
    }
    expect_identical(actual, fixture$value, info = spec$fixture)

    expect_no_warning(
        unclean <- call_encoder(spec, fixture$word, clean = FALSE)
    )
    expect_identical(
        unclean[valid],
        fixture$value[valid],
        info = paste(spec$fixture, "valid rows with clean = FALSE")
    )
    expect_false(
        anyNA(unclean[!valid]),
        info = paste(spec$fixture, "invalid rows with clean = FALSE")
    )
}

expect_scalar_vector_equivalence <- function(spec, input) {
    vector_result <- suppressWarnings(call_encoder(spec, input))
    scalar_result <- vapply(
        input,
        function(value) suppressWarnings(call_encoder(spec, value)),
        character(1),
        USE.NAMES = FALSE
    )

    expect_identical(vector_result, scalar_result, info = spec$name)
    expect_length(vector_result, length(input))
}

encoder_specs <- list(
    caverphone = list(
        name = "Caverphone",
        encoder = caverphone,
        args = list(),
        fixture = "caverphone.csv",
        max_code_len = 6L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    caverphone_modified = list(
        name = "Modified Caverphone",
        encoder = caverphone,
        args = list(modified = TRUE),
        fixture = "caverphone-modified.csv",
        max_code_len = 10L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    cologne = list(
        name = "Cologne",
        encoder = cologne,
        args = list(),
        fixture = "cologne.csv",
        max_code_len = NULL,
        alphabet = "^[0-8]*$",
        accepts_umlaut = TRUE
    ),
    lein = list(
        name = "LEIN",
        encoder = lein,
        args = list(),
        fixture = "lein.csv",
        max_code_len = 4L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    metaphone = list(
        name = "Metaphone",
        encoder = metaphone,
        args = list(),
        fixture = "metaphone.csv",
        max_code_len = 10L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    mra = list(
        name = "Match Rating Approach",
        encoder = mra_encode,
        args = list(),
        fixture = "mra-encode.csv",
        max_code_len = 6L,
        alphabet = "^[A-Z]*$",
        accepts_umlaut = FALSE
    ),
    nysiis = list(
        name = "NYSIIS",
        encoder = nysiis,
        args = list(),
        fixture = "nysiis.csv",
        max_code_len = 6L,
        alphabet = "^[A-Z]*$",
        accepts_umlaut = FALSE
    ),
    nysiis_modified = list(
        name = "Modified NYSIIS",
        encoder = nysiis,
        args = list(modified = TRUE),
        fixture = "nysiis-modified.csv",
        max_code_len = 6L,
        alphabet = "^[A-Z]*$",
        accepts_umlaut = FALSE
    ),
    onca = list(
        name = "ONCA",
        encoder = onca,
        args = list(),
        fixture = "onca.csv",
        max_code_len = 4L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    onca_modified = list(
        name = "Modified ONCA",
        encoder = onca,
        args = list(modified = TRUE),
        fixture = "onca-modified.csv",
        max_code_len = 4L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    onca_refined = list(
        name = "Refined ONCA",
        encoder = onca,
        args = list(refined = TRUE),
        fixture = "onca-refined.csv",
        max_code_len = 4L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    onca_modified_refined = list(
        name = "Modified refined ONCA",
        encoder = onca,
        args = list(modified = TRUE, refined = TRUE),
        fixture = "onca-modified-refined.csv",
        max_code_len = 4L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    phonex = list(
        name = "Phonex",
        encoder = phonex,
        args = list(),
        fixture = "phonex.csv",
        max_code_len = 4L,
        alphabet = "^[A-Z0-6]*$",
        accepts_umlaut = TRUE
    ),
    rogerroot = list(
        name = "Roger Root",
        encoder = rogerroot,
        args = list(),
        fixture = "rogerroot.csv",
        max_code_len = 5L,
        alphabet = "^[0-9]*$",
        accepts_umlaut = FALSE
    ),
    soundex = list(
        name = "Soundex",
        encoder = soundex,
        args = list(),
        fixture = "soundex.csv",
        max_code_len = 4L,
        alphabet = "^[A-Z0-6]*$",
        accepts_umlaut = FALSE
    ),
    soundex_refined = list(
        name = "Refined Soundex",
        encoder = refinedSoundex,
        args = list(),
        fixture = "soundex-refined.csv",
        max_code_len = 10L,
        alphabet = "^[A-Z0-9]*$",
        accepts_umlaut = FALSE
    ),
    statcan = list(
        name = "Statistics Canada",
        encoder = statcan,
        args = list(),
        fixture = "statcan.csv",
        max_code_len = 4L,
        alphabet = "^[A-Z]*$",
        accepts_umlaut = TRUE
    )
)

phonics_methods <- c(
    "caverphone", "caverphone.modified", "cologne", "lein", "metaphone",
    "nysiis", "nysiis.modified", "onca", "onca.modified", "onca.refined",
    "onca.modified.refined", "phonex", "rogerroot", "soundex",
    "soundex.refined", "statcan"
)

phonics_method_calls <- list(
    caverphone = function(x, clean) caverphone(x, clean = clean),
    caverphone.modified = function(x, clean) caverphone(x, modified = TRUE, clean = clean),
    cologne = function(x, clean) cologne(x, clean = clean),
    lein = function(x, clean) lein(x, clean = clean),
    metaphone = function(x, clean) metaphone(x, clean = clean),
    nysiis = function(x, clean) nysiis(x, clean = clean),
    nysiis.modified = function(x, clean) nysiis(x, modified = TRUE, clean = clean),
    onca = function(x, clean) onca(x, clean = clean),
    onca.modified = function(x, clean) onca(x, modified = TRUE, clean = clean),
    onca.refined = function(x, clean) onca(x, refined = TRUE, clean = clean),
    onca.modified.refined = function(x, clean) onca(x, modified = TRUE, refined = TRUE, clean = clean),
    phonex = function(x, clean) phonex(x, clean = clean),
    rogerroot = function(x, clean) rogerroot(x, clean = clean),
    soundex = function(x, clean) soundex(x, clean = clean),
    soundex.refined = function(x, clean) refinedSoundex(x, clean = clean),
    statcan = function(x, clean) statcan(x, clean = clean)
)

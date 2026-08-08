test_that("Caverphone 2 preserves the canonical initial-Y marker", {
    # Hood (2004), Caverphone 2.0 specification: initial y is replaced by A.
    expect_identical(caverphone("Y", modified = TRUE), "A111111111")
})

test_that("Caverphone 1 reproduces the report examples", {
    # Hood (2002), Caverphone 1.0 specification examples.
    expect_identical(
        caverphone(c("David", "Whittle", "Thompson")),
        c("TFT111", "WTL111", "TMPSN1")
    )
})

test_that("MRA comparison counts unmatched characters in the longer name", {
    # Moore et al. (1977), NBS SP 500-2, pp. 17-18, comparison steps 3-6.
    expect_false(mra_compare("A", "BB"))
    expect_true(mra_compare("A", "AA"))
})

test_that("MRA encoding follows the NBS six-character procedure", {
    # Moore et al. (1977), NBS SP 500-2, p. 17.
    expect_identical(
        mra_encode(c("Byrne", "Boern", "Catherine")),
        c("BYRN", "BRN", "CTHRN")
    )
})

test_that("NYSIIS follows the published contextual scan", {
    # Lynch and Arends (1977), Appendix B: W following a vowel becomes the
    # preceding character; interior EV becomes AF during the scan.
    expect_identical(nysiis("EWALD"), "EALD")
    expect_identical(nysiis("DEVITO", modified = TRUE), "DAFAT")
})

test_that("Modified NYSIIS applies the USDA modifications in scan order", {
    # Lynch and Arends (1977), Appendix B, modified rules 1-11.
    expect_identical(
        nysiis(c("RHODES", "HUNT", "HADDIX"), modified = TRUE),
        c("RAD", "HAN", "HADAC")
    )
    expect_identical(
        nysiis(c("LEIGHTON", "WOOLDRIDGE"), modified = TRUE),
        c("LATAN", "WALDRA")
    )
    expect_warning(
        expect_identical(nysiis("MAJR", modified = TRUE), NA_character_),
        "ending in JR or SR"
    )
})

test_that("Cologne evaluates C and X against their original neighbors", {
    # Postel (1969), rule table: C before X is 4 and X after C is 8.
    expect_identical(cologne(c("CX", "ACX")), c("48", "048"))
})

test_that("Lein applies the USDA deletion and conversion table", {
    # Lynch and Arends (1977), Appendix B, Lein rules 1-4.
    expect_identical(lein(c("Smith", "Schmidt")), c("S210", "S521"))
})

test_that("traditional Metaphone covers its contextual rules", {
    # Philips (1990), initial silent letters, X, and TH rules.
    expect_identical(
        metaphone(c("knight", "xerox", "the")),
        c("NFT", "SRKS", "0")
    )
})

test_that("original NYSIIS follows the published boundary rules", {
    # Taft (1970), reproduced in Lynch and Arends (1977), Appendix B.
    expect_identical(
        nysiis(c("MacIntosh", "Knuth", "Pfeister")),
        c("MCANT", "NAT", "FASTAR")
    )
})

test_that("ONCA variants compose the selected NYSIIS and Soundex stages", {
    # Gill (1997): ONCA is a NYSIIS stage followed by Soundex. The modified
    # and refined combinations are explicit phonics extensions.
    input <- c("Christopher", "Smith")
    intermediateLength <- max(nchar(input) + 1L)

    expect_identical(
        onca(input),
        soundex(nysiis(input, maxCodeLen = intermediateLength))
    )
    expect_identical(
        onca(input, modified = TRUE),
        soundex(nysiis(input, maxCodeLen = intermediateLength, modified = TRUE))
    )
    expect_identical(
        onca(input, refined = TRUE),
        refinedSoundex(nysiis(input, maxCodeLen = intermediateLength), maxCodeLen = 4)
    )
    expect_identical(
        onca(input, modified = TRUE, refined = TRUE),
        refinedSoundex(
            nysiis(input, maxCodeLen = intermediateLength, modified = TRUE),
            maxCodeLen = 4
        )
    )
})

test_that("Roger Root uses the published DC entry and retains earlier digits", {
    # Lynch and Arends (1977), Appendix B, first-letter/basic tables and
    # adjacent-equal-value rule.
    expect_identical(rogerroot(c("DC", "BDC", "ASS")), c("07000", "09700", "10000"))
})

test_that("Refined Soundex retains a one-letter name", {
    # Apache Commons Codec RefinedSoundex starts scanning after the retained
    # first letter, so a one-letter name has no appended mapping.
    expect_identical(refinedSoundex(c("A", "B")), c("A", "B"))
})

test_that("Statistics Canada normalization is case invariant", {
    # The documented French input domain is normalized before rule processing.
    expect_identical(statcan(c("éves", "ÉVES")), c("EVS", "EVS"))
})

test_that("census Soundex reproduces National Archives examples", {
    # U.S. National Archives Soundex guide, including H/W and vowel separators.
    expect_identical(
        soundex(c("Pfister", "Jackson", "Tymczak", "Ashcraft")),
        c("P236", "J250", "T522", "A261")
    )
})

test_that("Census Modified Statistics Canada follows the USDA rules", {
    # Lynch and Arends (1977), Appendix B, Census Canada rules 1-5.
    expect_identical(statcan(c("Daves", "Davies", "Devos")), rep("DVS", 3))
})

test_that("Phonex ignores only one D or G after M or N", {
    # Lait and Randell (1996), Appendix: ignore the next letter when it is D/G.
    expect_identical(phonex("AMDD"), "A530")
})

test_that("padding preserves valid all-zero Roger Root codes", {
    # Roger Root assigns initial S the two-digit first-table value 00.
    expect_identical(rogerroot("S"), "00000")
    expect_identical(rogerroot(""), "")
    expect_identical(lein("", maxCodeLen = 8), "")
})
